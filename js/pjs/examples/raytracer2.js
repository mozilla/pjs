// Copyright (c) 2012 Nicholas Matsakis.  This is a lightly modified variant
// of code produced by Chris Killpack and released under the MIT license.
// I am releasing this code under the same terms.  The original
// copyright notice is below:
//
// Copyright (c) 2010 Chris Killpack
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

/**
 * Container for all the scene lights.
 * @constructor
 */
function Lights() {
    /**
     * Holds the scene lights.
     * @type {Array.<!Light>}
     * @private
     */
    this.lights_ = new Array();
}


/**
 * Add a light to the scene.
 * @param {Light} light The light to be added.
 */
Lights.prototype.addLight = function(light) {
    this.lights_.push(light);
};


/**
 * Iterate over each light in the scene evaluating it's contribution to the
 * given point and executing the provided material context.
 * @param {Vector3} pos The world position of the point receiving the
 *     light.
 * @param {Vector3} normal The world normal of the point receiving the light.
 * @param {function(this:Material, irradiance)} closure Material shader
 *     callback executed for each light.
 * @param {Material} context The context the closure will run in.
 */
Lights.prototype.forEachLight = function(pos, normal, closure, context) {
    for (var i = 0; i < this.lights_.length; i++) {
        var irradiance = this.lights_[i].evaluateLight(pos, normal);
        closure.call(context, irradiance);
    }
};


/**
 * A POD structure that holds the result of a lighting calculation.
 * @param {Vector3} direction The direction of the irradiance on the point.
 * @param {Vector3} color The color and intensity of the irradiance.
 * @param {boolean} lightVisible Whether the light can see the point or not.
 * @constructor
 */
function Irradiance(direction, color, lightVisible) {
    /**
     * The direction of the irradiance on the point.
     * @type {Vector3}
     */
    this.direction = direction;

    /**
     * The color and intensity of the irradiance.
     * @type {Vector3}
     */
    this.color = color;

    /**
     * Whether the light can see the point.
     * @type {boolean}
     */
    this.lightVisible = lightVisible;
}


/**
 * Implements a directional light.
 * @param {Vector3} direction The normalized direction of the light.
 * @param {Vector3} color The color of the light.
 * @constructor
 */
function DirectionalLight(direction, color) {
    /**
     * The direction the light is shining in.
     * @type {Vector3}
     * @private
     */
    this.direction_ = direction;

    /**
     * The color of the light.
     * @type {Vector3}
     * @private
     */
    this.color_ = color;
}


/**
 * How large of a step to take when building the start point for the
 * shadow test.
 * @const
 * @private
 */
DirectionalLight.LIGHT_STEP_SIZE_ = 10000;


/**
 * Evaluate the directional light as it applies to the provided world
 * position.
 * @param {Vector3} pos The world position of the surface point.
 * @param {Vector3} normal The lighting normal in world space for the surface
 *     point.
 * @return {Irradiance} The result of the light evaluation for the input point.
 */
DirectionalLight.prototype.evaluateLight = function(pos, normal) {
    // Shadow test - can the surface point 'see' the light?
    // Directional lights don't have an source, but we need one for the shadow
    // test. In addition, directional lights don't have perspective shadows
    // so we take the point under consideration and take a very large step
    // along the light's direction 'towards' the light source.
    var lightPosition = Vector3.addMul(pos, this.direction_,
                                       -DirectionalLight.LIGHT_STEP_SIZE_);
    var ray = new Ray(lightPosition, this.direction_);
    var shadowTest = intersectRayWithScene(ray);
    // TODO: This test will be unnecessary once we move to testing ray segments.
    var lightVisible = Math.abs(shadowTest.t -
                                DirectionalLight.LIGHT_STEP_SIZE_) < 1e-2;

    return new Irradiance(this.direction_, this.color_, lightVisible);
};

/**
 * Base class that all materials must inherit from and implement two methods:
 *   evaluate()
 *       Handles the overall computation for the shading operation, including
 *       processing contributions from all lights.
 *   evaluateRadiance_()
 *       Compute the material's reaction to the irradiance.
 * @constructor
 */
function Material() {
}


/**
 * Evaluate the material with the shade context.
 * @param {ShadeContext} context The shade context.
 * @return {Vector3} The color of the material.
 */
Material.prototype.evaluate = function(context) {
    throw 'evaluate is not implemented.';
    return new Vector3();
};


/**
 * Compute the radiance of the material to the incoming light.
 * @param {Irradiance} irradiance The light reaching the point on the material.
 * @param {Vector3} pos The world space position of the point to be shaded.
 * @param {Vector3} normal The geometric normal of the shaded point in world
 *     space.
 * @param {ShadeContext} context The shade context.
 * @return {Vector3} Material reflectance.
 * @private
 */
Material.prototype.evaluateRadiance_ = function(irradiance, pos, normal,
                                                context) {
    throw 'evaluateRadiance_ is not implemented.';
    return new Vector3();
};


/**
 * Implements an ambient material.
 * @param {Vector3} color The material color.
 * @constructor
 */
function AmbientMaterial(color) {
    /**
     * @type {Vector3}
     * @private
     */
    this.color_ = color;
}
AmbientMaterial.prototype = new Material();


/**
 * Evaluate the material with the shade context.
 * @param {ShadeContext} context The shade context.
 * @return {Vector3} The color of the material.
 */
AmbientMaterial.prototype.evaluate = function(context) {
    return this.color_;
};


/**
 * Compute the radiance of the AmbientMaterial to the incoming light.
 * @param {Irradiance} irradiance The light reaching the point on the material.
 * @param {Vector3} pos The world space position of the point to be shaded.
 * @param {Vector3} normal The geometric normal of the shaded point in world
 *     space.
 * @param {ShadeContext} context The shade context.
 * @return {Vector3} Material reflectance.
 * @private
 */
AmbientMaterial.prototype.evaluateRadiance_ = function(irradiance, pos, normal,
                                                       context) {
    return this.color_;
};


/**
 * Implements a diffuse material.
 * @param {Vector3} color The diffuse color.
 * @param {?Vector3} opt_ambientColor The ambient color.
 * @constructor
 */
function DiffuseMaterial(color, opt_ambientColor) {
    /**
     * @type {Vector3}
     * @private
     */
    this.color_ = color;

    /**
     * @type {Vector3}
     * @private
     */
    this.ambientColor_ = opt_ambientColor || new Vector3();
}
DiffuseMaterial.prototype = new Material();


/**
 * Evaluate the material with the shade context.
 * @param {ShadeContext} context The shade context.
 * @return {Vector3} The color of the material.
 */
DiffuseMaterial.prototype.evaluate = function(context) {
    var worldPos = context.ray.pointOnRay(context.t);

    var color = new Vector3();
    g_lights.forEachLight(worldPos, context.normal, function(incidentLight) {
        // Compute the material's response to the incoming light.
        var c = this.evaluateRadiance_(incidentLight, worldPos, context.normal);
        color.add(c);
    }, this);
    var ambient = Vector3.componentScale(this.ambientColor_, this.color_);
    color.add(ambient);

    return color;
};


/**
 * Compute the reflectance of the material to the incoming light.
 * @param {Irradiance} irradiance The light reaching the point on the material.
 * @param {Vector3} pos The world space position of the point to be shaded.
 * @param {Vector3} normal The geometric normal of the point to be shaded.
 * @param {ShadeContext} context The shade context.
 * @return {Vector3} Material reflectance.
 * @private
 */
DiffuseMaterial.prototype.evaluateRadiance_ = function(irradiance, pos,
                                                       normal, context) {
    if (irradiance.lightVisible) {
        var lambert = Math.max(-Vector3.dot(normal,
                                            irradiance.direction), 0);
        var c = Vector3.scale(irradiance.color, lambert);
        c.componentScale(this.color_);
        return c;
    } else {
        return new Vector3();
    }
};


/**
 * Implements a specular material.
 * @param {Vector3} specularColor The specular color of the surface.
 * @param {number} specularPower The shininess of the surface.
 * @param {DiffuseMaterial} diffuseMaterial The diffuse material component.
 * @constructor
 */
function SpecularMaterial(specularColor, specularPower, diffuseMaterial) {
    /**
     * @type {Vector3}
     * @private
     */
    this.specularColor_ = specularColor;

    /**
     * @type {number}
     * @private
     */
    this.specularPower_ = specularPower;

    /**
     * @type {Material}
     * @private
     */
    this.diffuseMaterial_ = diffuseMaterial;
}
SpecularMaterial.prototype = new Material();


/**
 * Evaluate the material with the shade context.
 * @param {ShadeContext} context The shade context.
 * @return {Vector3} The color of the material.
 */
SpecularMaterial.prototype.evaluate = function(context) {
    var worldPos = context.ray.pointOnRay(context.t);

    var diffuseColor = new Vector3();
    var specularColor = new Vector3();
    g_lights.forEachLight(worldPos, context.normal, function(incidentLight) {
        // Compute the diffuse material's reaction to the incident light.
        diffuseColor.add(
            this.diffuseMaterial_.evaluateRadiance_(
                incidentLight, worldPos, context.normal, context));

        // Compute the specular component's reaction to the incident light.
        specularColor.add(
            this.evaluateRadiance_(incidentLight, worldPos, context.normal,
                                   context));
    }, this);

    return Vector3.add(diffuseColor, specularColor);
};


/**
 * Compute the reflectance of the material to the incoming light.
 * @param {Irradiance} irradiance The light reaching the point on the material.
 * @param {Vector3} pos The world position of the shaded point.
 * @param {Vector3} normal The geometric normal of the shaded point.
 * @param {ShadeContext} context The shade context.
 * @return {Vector3} Material reflectance.
 * @private
 */
SpecularMaterial.prototype.evaluateRadiance_ = function(irradiance, pos,
                                                        normal, context) {
    var r = Vector3.reflect(context.ray.direction, normal);
    var pow = Math.pow(Math.max(-Vector3.dot(r, irradiance.direction), 0),
                       this.specularPower_);
    return Vector3.scale(irradiance.color, pow);
};


/**
 * Implements a checkerboard material.
 * @param {Material} material1 The material of one set of squares.
 * @param {Material} material2 The material of the other set of squares.
 * @param {number} size The size of the squares.
 * @constructor
 */
function CheckerMaterial(material1, material2, size) {
    this.material1 = material1;
    this.material2 = material2;
    this.size = size;
}

/**
 * Evaluate the material with the shade context.
 * @param {ShadeContext} context The shade context.
 * @return {Vector3} The color of the material.
 */
CheckerMaterial.prototype.evaluate = function(context) {
    // Compute the world position
    var worldPos = context.ray.pointOnRay(context.t);
    var x = Math.floor(worldPos.x / 4) & 1;
    var z = Math.floor(worldPos.z / 4) & 1;
    var material = (x ^ z) ? this.material1 : this.material2;
    return material.evaluate(context);
};

/**
 * The base class for all scene objects.
 * @param {Material} material The object's material.
 * @constructor
 */
function SceneObject(material) {
    /**
     * @type {Material}
     * @private
     */
    this.material_ = material;
}


/**
 * Invoke the object's material to shade the intersection point.
 * @param {RayContext} context The constructed context of the ray
 *     intersection.
 * @return {Vector3} The color output of the shade operation.
 */
SceneObject.prototype.shade = function(context) {
    return this.material_.evaluate(context);
};


/**
 * Perform a ray-object intersection test. This function must be implemented
 * by all classes that inherit from SceneObject.
 * @param {Ray} ray The ray to be tested.
 * @return {{t: number, normal: Vector3}} t holds the parametric distance along
 *     the ray to the closest point of intersection with the object, normal
 *     holds the object normal at the point of intersection. If there is no
 *     intersection then undefined is returned.
 */
SceneObject.prototype.intersect = function(ray) {
    throw 'intersect is not implemented.';
};


/**
 * Implement a Sphere primitive.
 * @param {Vector3} center The center of the sphere.
 * @param {number} radius The radius of the sphere.
 * @param {Material} material The material that covers the sphere.
 * @constructor
 */
function Sphere(center, radius, material) {
    SceneObject.call(this, material);

    /**
     * @type {Vector3}
     * @private
     */
    this.center_ = center;

    /**
     * @type {number}
     * @private
     */
    this.radius_ = radius;
}
Sphere.prototype = new SceneObject();

/**
 * Perform a ray-sphere intersection test.
 * @param {Ray} ray The ray to be tested.
 * @return {{t: number, normal: Vector3}} t holds the parametric distance along
 *     the ray to the closest point of intersection with the sphere, normal
 *     holds the sphere normal at the point of intersection. If there is no
 *     intersection then undefined is returned.
 */
Sphere.prototype.intersect = function(ray) {
    /**
     * From: http://www.cs.umbc.edu/~olano/435f02/ray-sphere.html
     */
    var dst = Vector3.subtract(ray.origin, this.center_);

    var a = Vector3.dot(ray.direction, ray.direction);
    var b = 2 * Vector3.dot(ray.direction, dst);
    var c = Vector3.dot(dst, dst) - (this.radius_ * this.radius_);

    var discrim_sq = b * b - 4 * a * c;
    if (discrim_sq < 0) {
        return undefined;
    }

    var discrim = Math.sqrt(discrim_sq);
    if (Math.abs(discrim_sq) > 1e-2) {
        // Two intersections, return the closer one. For reference the other is at
        // (-b + discrim) / (2 * a).
        var t = (-b - discrim) / (2 * a);
    } else {
        // Glancing intersection, with one solution.
        var t = -b / (2 * a);
    }

    return {
        t: t,
        normal: this.normal(ray, t)
    };
};


/**
 * Compute the normal of the sphere for the intersection point.
 * @param {Ray} ray The intersecting ray.
 * @param {number} t The parametric point of intersection.
 * @return {Vector3} The normal at the point of intersection.
 */
Sphere.prototype.normal = function(ray, t) {
    var worldPos = ray.pointOnRay(t);
    var v = Vector3.subtract(worldPos, this.center_);
    return v.normalize();
};


/**
 * Implements a plane primitive.
 * @param {Vector3} normal The plane normal.
 * @param {number} offset The distance of the plane along the normal from
 *   the origin.
 * @param {Material} material The plane surface material.
 * @constructor
 */
function Plane(normal, offset, material) {
    SceneObject.call(this, material);

    /**
     * @type {Vector3} normal
     * @private
     */
    this.normal_ = normal;

    /**
     * @type {number}
     * @private
     */
    this.offset_ = offset;
}
Plane.prototype = new SceneObject();


/**
 * Test for an intersection between the ray and the plane.
 * @param {Ray} ray The ray to intersect with the plane.
 * @return {{t: number, normal: Vector3}} t holds the parametric distance along
 *     the ray to the closest point of intersection with the plane, normal
 *     holds the plane normal at the point of intersection. If there is no
 *     intersection then undefined is returned.
 */
Plane.prototype.intersect = function(ray) {
    /**
     * From https://www.siggraph.org/education/materials/HyperGraph/raytrace/rayplane_intersection.htm
     */
    var Vd = Vector3.dot(this.normal_, ray.direction);
    if (Math.abs(Vd) < 1e-2) {
        // Parallel to the plane, no intersection
        return undefined;
    }
    var V0 = -(Vector3.dot(this.normal_, ray.origin) - this.offset_);
    var t = V0 / Vd;
    if (t < 0) {
        // Intersection is behind ray origin, ignore.
        return undefined;
    }

    return {
        t: t,
        normal: this.normal_
    };
};


/**
 * @param {number} width The width of the box.
 * @param {number} height The height of the box.
 * @param {number} depth The depth of the box.
 * @param {Vector3} center The center of the box.
 * @param {Material} material The box's material.
 * @constructor
 */
function Box(width, height, depth, center, material) {
    SceneObject.call(this, material);

    var width2 = width / 2;
    var height2 = height / 2;
    var depth2 = depth / 2;

    /**
     * The 'minimal' corner of the AABB.
     * @type {Vector3}
     * @private
     */
    this.p0_ = Vector3.subtract(center, new Vector3(width2, height2, depth2));

    /**
     * The 'maximal' corner of the AABB.
     * @type {Vector3}
     * @private
     */
    this.p1_ = Vector3.add(center, new Vector3(width2, height2, depth2));
}
Box.prototype = new SceneObject();


/**
 * Test for an intersection between the ray and the box.
 * @param {Ray} ray The ray to intersect with the box.
 * @return {Array.<number>} The array of the ray's parametrics values at
 *     points of intersection with the plane.
 */
Box.prototype.intersect = function(ray) {
    if (ray.direction.x >= 0) {
        var txMin = (this.p0_.x - ray.origin.x) / ray.direction.x;
        var txMax = (this.p1_.x - ray.origin.x) / ray.direction.x;
    } else {
        var txMin = (this.p1_.x - ray.origin.x) / ray.direction.x;
        var txMax = (this.p0_.x - ray.origin.x) / ray.direction.x;
    }

    if (ray.direction.y >= 0) {
        var tyMin = (this.p0_.y - ray.origin.y) / ray.direction.y;
        var tyMax = (this.p1_.y - ray.origin.y) / ray.direction.y;
    } else {
        var tyMin = (this.p1_.y - ray.origin.y) / ray.direction.y;
        var tyMax = (this.p0_.y - ray.origin.y) / ray.direction.y;
    }

    if (ray.direction.z >= 0) {
        var tzMin = (this.p0_.z - ray.origin.z) / ray.direction.z;
        var tzMax = (this.p1_.z - ray.origin.z) / ray.direction.z;
    } else {
        var tzMin = (this.p1_.z - ray.origin.z) / ray.direction.z;
        var tzMax = (this.p0_.z - ray.origin.z) / ray.direction.z;
    }

    // Find the biggest of txMin, tyMin and tzMin.
    // Also tracks the normal of the intersecting face.
    var t0 = txMin;
    var normal = new Vector3(-Box.sign_(ray.direction.x), 0, 0);
    if (t0 < tyMin) {
        t0 = tyMin;
        normal = new Vector3(0, -Box.sign_(ray.direction.y), 0);
    }
    if (t0 < tzMin) {
        t0 = tzMin;
        normal = new Vector3(0, 0, -Box.sign_(ray.direction.z));
    }

    // Find the smallest of txMax, tyMax and tzMax.
    var t1 = Math.min(txMax, Math.min(tyMax, tzMax));
    if (t0 < t1) {
        // Intersection. The two points of intersection are [t0, t1], but only
        // the closer point is returned.
        return {
            t: t0,
            normal: normal
        };
    }

    // No intersection.
    return undefined;
};


/**
 * Return the sign of a number.
 * @param {number} x The number to be tested.
 * @return {number} -1 if the input is negative, 1 if it is positive, 0
 *     otherwise.
 * @private
 */
Box.sign_ = function(x) {
    if (Math.abs(x) < 1e-5) {
        return 0;
    }

    return (x < 0) ? -1 : 1;
};

RT = {
		Infinity: Infinity,
		Undefined: undefined
} 

/**
 * Test all objects in the scene for intersection with the ray.
 * @param {Ray} ray The ray to intersect with the scene.
 * @return {{t: number, normal: Vector3, obj: SceneObject}} The closest intersection
 *     along the ray, the normal at the point of intersection and the object
 *     that intersected the ray, or undefined if the ray does not intersect
 *     any objects.
 */
// TODO: Make this a method of an scene container.
// TODO: This function should test ray segments against objects for
// intersection.
function intersectRayWithScene(ray) {
    var closest_t = RT.Infinity;
    var closest_obj = RT.Undefined;
    var closest_intersection = RT.Undefined;
    for (var objectIdx = 0; objectIdx < g_objects.length; objectIdx++) {
        var intersect = g_objects[objectIdx].intersect(ray);
        if (intersect && intersect.t < closest_t) {
            closest_t = intersect.t;
            closest_obj = g_objects[objectIdx];
            closest_intersection = intersect;
        }
    }

    if (closest_obj === RT.Undefined) {
        return RT.Undefined;
    }

    return {
        t: closest_t,
        normal: closest_intersection.normal,
        obj: closest_obj
    };
}

/**
 * A class that implements the ray.
 * @param {Vector3} origin The origin of the ray.
 * @param {Vector3} direction The direction of the ray.
 * @constructor
 */
function Ray(origin, direction) {
    this.origin = origin;

    this.direction = direction;
}


/**
 * Compute a position along the ray for a given parametric value t.
 * @param {Number} t The parametric value.
 * @return {Vector3} Position along the ray.
 */
Ray.prototype.pointOnRay = function(t) {
    return Vector3.addMul(this.origin, this.direction, t);
};

var g_width;
var g_height;

function init(width, height) {
    g_width = width;
    g_height = height;
    initScene();
}


/**
 * Convert a Vector3 into a CSS color string.
 * @param {Vector3} c Color.
 * @return {string} CSS rgb color.
 */
function colorToString(c) {
    return 'rgb(' + Math.floor(c.x) + ',' + Math.floor(c.y) + ','
        + Math.floor(c.z) + ')';
}


/**
 * Set a pixel in the canvas to the specified color.
 * @param {CanvasRenderingContext2D} context The 2D rendering context for a
 *     canvas element.
 * @param {number} x The x co-ordinate of the pixel.
 * @param {number} y The y co-ordinate of the pixel.
 * @param {Vector3} color The color to set the pixel.
 */
function toRGB(color) {
//    var c = Vector3.copyFrom(color);
	var c = new Vector3(color.x, color.y, color.z);
    c.clamp().componentScale(new Vector3(255, 255, 255));
    return c;
}

function asciiDump(context, width, height) {
    // Rather arbitrary progression from light to dark in 10 steps:
    var colors = [" ", ".", ":",
                  "|", "~", "o",
                  "O", "*", "%",
                  "#"];
    colors.reverse(); // high values are lighter
    var str = "";
    for (var y = 0; y < height; y++) {
        for (var x = 0; x < width; x++) {
            var c = context[y][x];
            var avg = (c.x + c.y + c.z) / 3;
            var ten = (avg * 10 / 255) >> 0;
            str += colors[ten];
        }
        str += "\n";
    }
    print(str);
}

/**
 * Initialize the scene.
 */
function initScene() {
    g_lights = new Lights();
    g_lights.addLight(new DirectionalLight(new Vector3(0, -1, 0),
                                           new Vector3(1, 1, 1)));

    g_objects = new Array();

    var redDiffuse = new DiffuseMaterial(new Vector3(1, 0, 0),
                                         new Vector3(0.1, 0, 0));
    var redSpecular = new SpecularMaterial(new Vector3(1, 1, 1), 8, redDiffuse);
    var sphere = new Sphere(new Vector3(0, 0, 10), 5, redSpecular);
    g_objects.push(sphere);

    var yellowCheck = new DiffuseMaterial(new Vector3(1, 1, 0),
                                          new Vector3(0.4, 0.4, 0));
    var blueCheck = new DiffuseMaterial(new Vector3(0, 0, 1),
                                        new Vector3(0, 0, 0.4));
    var plane = new Plane(new Vector3(0, 1, 0), -4,
                          new CheckerMaterial(yellowCheck, blueCheck));
    g_objects.push(plane);

    var box = new Box(2, 2, 2, new Vector3(-3.5, -2.5, 1),
                      new DiffuseMaterial(new Vector3(0, 1, 0),
                                          new Vector3(0, 0.2, 0)));
    g_objects.push(box);

    g_sphere = sphere;

    g_eye_origin = new Vector3(0, 0, -10);
    g_near_plane = 0;  // z coordinate
}


/**
 * Construct a ray that passes through the screen pixel position.
 * @param {number} sx X component of the screen co-ordinate.
 * @param {number} sy Y component of the screen co-ordinate.
 * @return {Ray} A ray that passes through the screen pixel.
 */
function buildRay(sx, sy) {
    var width2 = g_width / 2;
    var height2 = g_height / 2;

    var x = ((sx - width2) / width2) * 5;
    var y = ((height2 - sy) / height2) * 5;
    var d = new Vector3(x, y,
                        g_near_plane - g_eye_origin.z);
    d.normalize();
    return new Ray(g_eye_origin, d);
}

function draw_row(i) {
    var sampler = new Sampler(1);
//    var i_pixels = new Array(g_width);
    var i_pixels = [];
    for (var j = 0; j < g_width; j++) {
        sampler.reset();

        while (sampler.hasNext()) {
            var samplePoint = sampler.next();

            // Build ray
//            var p = Vector2.add(new Vector2(j, i), samplePoint);

            var p = new Vector2( j + samplePoint.x, i + samplePoint.y);
            
            var ray = buildRay(j + samplePoint.x, i + samplePoint.y);

            // Trace ray against objects in the scene
            var intersection = intersectRayWithScene(ray);

            // If there was an intersection compute the shading.
            if (intersection) {
                var shadeContext = {
                    object: intersection.obj,
                    ray: ray,
                    t: intersection.t,
                    normal: intersection.normal
                };
                var color = intersection.obj.shade(shadeContext);
                sampler.accumulateSample(color);
            }
        }
        var pixelColor = sampler.result();
        i_pixels[j] = toRGB(pixelColor);
    }
    return i_pixels;
}

/**
 * Draw the scene.
 */
function draw(nxt) {
    var width = g_width;
    var height = g_height;

    var tasks = new Array(g_height);
    for (var i = 0; i < g_height; i++) {
        tasks[i] = fork(draw_row, i);
    }

    oncompletion(function() {
        var image = new Array(g_height);
        for (var i = 0; i < g_height; i++) {
            image[i] = tasks[i].get();
        }
        nxt(image);
    });
}

/**
 * Class to control image pixel sampling. Uses an iterator style API.
 * Inherit from this class to implement different sampling strategies.
 * Intended use:
 *   [for each pixel]
 *   sampler.reset()
 *   while (sampler.hasNext()) {
 *     var sampleCoordinate = sampler.next();
 *     var imagePlaneCoordinate = imagePlanePixel + sampleCoordinate
 *     ...
 *     sampler.accumulateSample(color)
 *   }
 *   sampler.result()
 * @param {number} numSamples The number of samples per pixel that the image
 *     sampler should take.
 * @constructor
 */
function Sampler(numSamples) {
    /**
     * @type {number}
     * @private
     */
    this.numSamples_ = numSamples;

    /**
     * @type {number}
     * @private
     */
    this.remainingSamples_ = numSamples;

    /**
     * @type {Vector3}
     * @private
     */
    this.colorAccumulator_ = new Vector3();
}


/**
 * Does the sampler want another sample point?
 * @return {boolean} True if there is another sample point, False otherwise.
 */
Sampler.prototype.hasNext = function() {
    return this.remainingSamples_ > 0;
};


/**
 * Returns the next sample point within the pixel, or undefined if the
 * iterator is expired.
 * @return {Vector2} The sample point.
 */
Sampler.prototype.next = function() {
    if (this.remainingSamples_ <= 0) {
        return undefined;
    }

    this.remainingSamples_ = this.remainingSamples_ - 1;

    // Setup state for accumulateSample().
    // For now we use a box filter.
    this.sampleWeight = 1 / this.numSamples_;

    return new Vector2(0, 0);
};


/**
 * Reset the sampler for the new image pixel.
 */
Sampler.prototype.reset = function() {
    // Reset for the next pixel.
    this.remainingSamples_ = this.numSamples_;
    this.colorAccumulator_.set(0, 0, 0);
};


/**
 * Add in the color contribution of this image sample. The contribution
 * to the final result is weighted.
 * @param {Vector3} color The color for this sample.
 */
Sampler.prototype.accumulateSample = function(color) {
    this.colorAccumulator_.addMul(color, this.sampleWeight);
};


/**
 * Retrieve the final color of the pixel.
 * @return {Vector3} Pixel color.
 */
Sampler.prototype.result = function() {
    return this.colorAccumulator_;
};

/**
 * A simple 3D vector class.
 */

/**
 * Create a new Vector3.
 * @param {?number} opt_x The x component.
 * @param {?number} opt_y The y component.
 * @param {?number} opt_z The z component.
 * @constructor
 */
function Vector3(opt_x, opt_y, opt_z) {
    this.set(opt_x || 0, opt_y || 0, opt_z || 0);
}


/**
 * Sets the components of the vector.
 * @param {number} x The x component.
 * @param {number} y The y component.
 * @param {number} z The z component.
 */
Vector3.prototype.set = function(x, y, z) {
    this.x = x;
    this.y = y;
    this.z = z;
};


/**
 * Create a new Vector3 instance from another.
 * @param {Vector3} v The vector to duplicate.
 * @return {Vector3} A new instance that duplicates v.
 */
Vector3.copyFrom = function(v) {
    return new Vector3(v.x, v.y, v.z);
};


/**
 * Normalize the vector to unit-length.
 * @return {Vector3} this To allow for chaining.
 */
Vector3.prototype.normalize = function() {
    var length = this.length();
    if (length > 1e-4) {
        this.x /= length;
        this.y /= length;
        this.z /= length;
    }

    return this;
};


/**
 * Compute the dot product of two vectors.
 * @param {Vector3} a A vector.
 * @param {Vector3} b Another vector.
 * @return {number} The dot product of a and b.
 */
Vector3.dot = function(a, b) {
    return a.x * b.x + a.y * b.y + a.z * b.z;
};


/**
 * Subtract two Vector3's and returns the result in a new Vector3.
 * @param {Vector3} a The first vector.
 * @param {Vector3} b The second vector.
 * @return {Vector3} The new Vector3.
 */
Vector3.subtract = function(a, b) {
    return new Vector3(a.x - b.x, a.y - b.y, a.z - b.z);
};


/**
 * Add two Vector3's and returns the result in a new Vector3.
 * @param {Vector3} a The first vector.
 * @param {Vector3} b The second vector.
 * @return {Vector3} The new Vector3.
 */
Vector3.add = function(a, b) {
    return new Vector3(a.x + b.x, a.y + b.y, a.z + b.z);
};


/**
 * Add another Vector3 to this Vector3.
 * @param {Vector3} v The vector to be added.
 * @return {Vector3} This instance, to allow for chaining.
 */
Vector3.prototype.add = function(v) {
    this.x += v.x;
    this.y += v.y;
    this.z += v.z;
    return this;
};


/**
 * Scale the components of the vector by a scalar factor.
 * @param {number} factor The scale factor.
 * @return {Vector3} The instance, to allow for chaining.
 */
Vector3.prototype.scale = function(factor) {
    this.x *= factor;
    this.y *= factor;
    this.z *= factor;
    return this;
};


/**
 * Create a new vector that is a scaled copy of the input.
 * @param {Vector3} v The vector to be scaled and copied.
 * @param {number} factor The scale factor.
 * @return {Vector3} A scaled copy of the input vector.
 */
Vector3.scale = function(v, factor) {
    return new Vector3(v.x * factor,
                       v.y * factor,
                       v.z * factor);
};


/**
 * Perform a component-wise scaling of the input vector.
 * @param {Vector3} v The vector to be scaled.
 * @param {Vector3} factor Per-component scale factors.
 * @return {Vector3} A copy of the input vector with it's components scaled.
 */
Vector3.componentScale = function(v, factor) {
    return new Vector3(v.x * factor.x,
                       v.y * factor.y,
                       v.z * factor.z);
};


/**
 * Perform a component-wise scaling of the vector.
 * @param {Vector3} factor Per-component scale factors.
 * @return {Vector3} this To allow for chaining.
 */
Vector3.prototype.componentScale = function(factor) {
    this.x *= factor.x;
    this.y *= factor.y;
    this.z *= factor.z;
    return this;
};


/**
 * Performs a scaled addition of another vector to this instance.
 * @param {Vector3} v The vector to be added.
 * @param {number} factor The scale factor.
 * @return {Vector3} The instance, to allow for chaining.
 */
Vector3.prototype.addMul = function(v, factor) {
    this.x += v.x * factor;
    this.y += v.y * factor;
    this.z += v.z * factor;
    return this;
};


/**
 * Returns a Vector3 that holds the result of adding a scaled copy of one
 * vector to another: a + b * factor.
 * @param {Vector3} a The vector to be added to.
 * @param {Vector3} b The vector to be scaled and then added.
 * @param {number} factor The scale factor.
 * @return {Vector3} The result.
 */
Vector3.addMul = function(a, b, factor) {
    return new Vector3(a.x + b.x * factor,
                       a.y + b.y * factor,
                       a.z + b.z * factor);
};


/**
 * Return the negated copy of a vector.
 * @param {Vector3} v The input vector.
 * @return {Vector3} A copy of the input vector, with each component negated.
 */
Vector3.negate = function(v) {
    return new Vector3(-v.x, -v.y, -v.z);
};


/**
 * Compute the length of the vector.
 * @return {number} The length of the vector.
 */
Vector3.prototype.length = function() {
    return Math.sqrt(this.x * this.x + this.y * this.y + this.z * this.z);
};


/**
 * Clamps the components of this Vector3 within the specified limits.
 * @param {?number} opt_min The minimum value a component can have. Defaults
 *     to 0.
 * @param {?number} opt_max The maximum value a component can have. Defaults
 *     to 1.
 * @return {Vector3} This to allow for chaining.
 */
Vector3.prototype.clamp = function(opt_min, opt_max) {
    var min = opt_min || 0;
    var max = opt_max || 1;

    this.x = Math.max(min, Math.min(this.x, max));
    this.y = Math.max(min, Math.min(this.y, max));
    this.z = Math.max(min, Math.min(this.z, max));
    return this;
};


/**
 * Reflect the vector v around another vector n.
 * Assumes that n is unit-length.
 * @param {Vector3} v The vector to be reflected.
 * @param {Vector3} n The vector to reflect around.
 * @return {Vector3} The reflected vector.
 */
Vector3.reflect = function(v, n) {
    // Formula: v' = v - 2 * n * v . n
    var scaledN = Vector3.copyFrom(n);
    scaledN.scale(Vector3.dot(v, n) * 2);
    return Vector3.subtract(v, scaledN);
};


/**
 * A 2D vector class.
 * @param {number} x The x component.
 * @param {number} y The y component.
 * @constructor
 */
function Vector2(x, y) {
//    this.set(x, y);
	this.x = x;
	this.y = y;
}


/**
 * Directly sets the individual components of this instance.
 * @param {number} x The x component.
 * @param {number} y The y component.
 * @return {Vector2} This instance, to allow for chaining.
 */
Vector2.prototype.set = function(x, y) {
    this.x = x;
    this.y = y;

    return this;
};


/**
 * Adds two vectors together and returns the result in a new vector.
 * @param {Vector2} a One of the vectors to be added.
 * @param {Vector2} b The other vector to be added.
 * @return {Vector2} The result of the addition operation.
 */
Vector2.add = function(a, b) {
    return new Vector2(a.x + b.x, a.y + b.y);
};

/**
 * Adds another vector into this instance.
 * @param {Vector2} p The vector to be added.
 * @return {Vector2} This instance, to allow for chaining.
 */
Vector2.prototype.add = function(p) {
    this.x += p.x;
    this.y += p.y;

    return this;
};

function run(width, height) {
    init(width, height);
    draw(function(img) {
        asciiDump(img, width, height);
    });
}

run(80, 40);