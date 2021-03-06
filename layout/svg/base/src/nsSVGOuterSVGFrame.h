/* -*- Mode: C++; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#ifndef __NS_SVGOUTERSVGFRAME_H__
#define __NS_SVGOUTERSVGFRAME_H__

#include "gfxMatrix.h"
#include "nsISVGSVGFrame.h"
#include "nsSVGContainerFrame.h"

class nsSVGForeignObjectFrame;

////////////////////////////////////////////////////////////////////////
// nsSVGOuterSVGFrame class

typedef nsSVGDisplayContainerFrame nsSVGOuterSVGFrameBase;

class nsSVGOuterSVGFrame : public nsSVGOuterSVGFrameBase,
                           public nsISVGSVGFrame
{
  friend nsIFrame*
  NS_NewSVGOuterSVGFrame(nsIPresShell* aPresShell, nsStyleContext* aContext);
protected:
  nsSVGOuterSVGFrame(nsStyleContext* aContext);

public:
  NS_DECL_QUERYFRAME
  NS_DECL_FRAMEARENA_HELPERS

#ifdef DEBUG
  ~nsSVGOuterSVGFrame() {
    NS_ASSERTION(mForeignObjectHash.Count() == 0,
                 "foreignObject(s) still registered!");
  }
#endif

  // nsIFrame:
  virtual nscoord GetMinWidth(nsRenderingContext *aRenderingContext);
  virtual nscoord GetPrefWidth(nsRenderingContext *aRenderingContext);

  virtual IntrinsicSize GetIntrinsicSize();
  virtual nsSize  GetIntrinsicRatio();

  virtual nsSize ComputeSize(nsRenderingContext *aRenderingContext,
                             nsSize aCBSize, nscoord aAvailableWidth,
                             nsSize aMargin, nsSize aBorder, nsSize aPadding,
                             PRUint32 aFlags) MOZ_OVERRIDE;

  NS_IMETHOD Reflow(nsPresContext*          aPresContext,
                    nsHTMLReflowMetrics&     aDesiredSize,
                    const nsHTMLReflowState& aReflowState,
                    nsReflowStatus&          aStatus);

  NS_IMETHOD  DidReflow(nsPresContext*   aPresContext,
                        const nsHTMLReflowState*  aReflowState,
                        nsDidReflowStatus aStatus);

  NS_IMETHOD BuildDisplayList(nsDisplayListBuilder*   aBuilder,
                              const nsRect&           aDirtyRect,
                              const nsDisplayListSet& aLists);

  NS_IMETHOD Init(nsIContent*      aContent,
                  nsIFrame*        aParent,
                  nsIFrame*        aPrevInFlow);

  virtual nsSplittableType GetSplittableType() const;

  /**
   * Get the "type" of the frame
   *
   * @see nsGkAtoms::svgOuterSVGFrame
   */
  virtual nsIAtom* GetType() const;

  void Paint(const nsDisplayListBuilder* aBuilder,
             nsRenderingContext* aContext,
             const nsRect& aDirtyRect);

#ifdef DEBUG
  NS_IMETHOD GetFrameName(nsAString& aResult) const
  {
    return MakeFrameName(NS_LITERAL_STRING("SVGOuterSVG"), aResult);
  }
#endif

  NS_IMETHOD  AttributeChanged(PRInt32         aNameSpaceID,
                               nsIAtom*        aAttribute,
                               PRInt32         aModType);

  virtual bool IsSVGTransformed(gfxMatrix *aOwnTransform,
                                gfxMatrix *aFromParentTransform) const {
    // Outer-<svg> can transform its children with viewBox, currentScale and
    // currentTranslate, but it itself is not transformed by SVG transforms.
    return false;
  }

  // nsISVGSVGFrame interface:
  virtual void NotifyViewportOrTransformChanged(PRUint32 aFlags);

  // nsSVGContainerFrame methods:
  virtual gfxMatrix GetCanvasTM(PRUint32 aFor);

  /* Methods to allow descendant nsSVGForeignObjectFrame frames to register and
   * unregister themselves with their nearest nsSVGOuterSVGFrame ancestor. This
   * is temporary until display list based invalidation is impleented for SVG.
   * Maintaining a list of our foreignObject descendants allows us to search
   * them for areas that need to be invalidated, without having to also search
   * the SVG frame tree for foreignObjects. This is important so that bug 539356
   * does not slow down SVG in general (only foreignObjects, until bug 614732 is
   * fixed).
   */
  void RegisterForeignObject(nsSVGForeignObjectFrame* aFrame);
  void UnregisterForeignObject(nsSVGForeignObjectFrame* aFrame);

  virtual bool HasChildrenOnlyTransform(gfxMatrix *aTransform) const;

#ifdef XP_MACOSX
  bool BitmapFallbackEnabled() const {
    return mEnableBitmapFallback;
  }
  void SetBitmapFallbackEnabled(bool aVal) {
    NS_NOTREACHED("don't think me need this any more"); // comment in bug 732429 if we do
    mEnableBitmapFallback = aVal;
  }
#endif

  /**
   * Return true only if the height is unspecified (defaulting to 100%) or else
   * the height is explicitly set to a percentage value no greater than 100%.
   */
  bool VerticalScrollbarNotNeeded() const;

  bool IsCallingUpdateBounds() const {
    return mCallingUpdateBounds;
  }

  void InvalidateSVG(const nsRegion& aRegion)
  {
    if (!aRegion.IsEmpty()) {
      mInvalidRegion.Or(mInvalidRegion, aRegion);
      InvalidateFrame();
    }
  }
  
  void ClearInvalidRegion() { mInvalidRegion.SetEmpty(); }

  const nsRegion& GetInvalidRegion() {
    if (!IsInvalid()) {
      mInvalidRegion.SetEmpty();
    }
    return mInvalidRegion;
  }

  nsRegion FindInvalidatedForeignObjectFrameChildren(nsIFrame* aFrame);

protected:

  bool mCallingUpdateBounds;

  /* Returns true if our content is the document element and our document is
   * embedded in an HTML 'object', 'embed' or 'applet' element. Set
   * aEmbeddingFrame to obtain the nsIFrame for the embedding HTML element.
   */
  bool IsRootOfReplacedElementSubDoc(nsIFrame **aEmbeddingFrame = nsnull);

  /* Returns true if our content is the document element and our document is
   * being used as an image.
   */
  bool IsRootOfImage();

  // This is temporary until display list based invalidation is implemented for
  // SVG.
  // A hash-set containing our nsSVGForeignObjectFrame descendants. Note we use
  // a hash-set to avoid the O(N^2) behavior we'd get tearing down an SVG frame
  // subtree if we were to use a list (see bug 381285 comment 20).
  nsTHashtable<nsPtrHashKey<nsSVGForeignObjectFrame> > mForeignObjectHash;

  nsAutoPtr<gfxMatrix> mCanvasTM;

  nsRegion mInvalidRegion; 

  float mFullZoom;

  bool mViewportInitialized;
#ifdef XP_MACOSX
  bool mEnableBitmapFallback;
#endif
  bool mIsRootContent;
};

#endif
