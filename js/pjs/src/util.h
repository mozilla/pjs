namespace pjs {

template<class T>
class auto_arr
{
private:
    T* ap;    // refers to the actual owned object (if any)

public:
    typedef T element_type;

    // constructor
    explicit auto_arr (T* ptr = 0) throw() : ap(ptr) { }

    // copy constructors (with implicit conversion)
    // - note: nonconstant parameter
    auto_arr (auto_arr& rhs) throw() : ap(rhs.release()) { }

    template<class Y>
    auto_arr (auto_arr<Y>& rhs) throw() : ap(rhs.release()) { }

    // assignments (with implicit conversion)
    // - note: nonconstant parameter
    auto_arr& operator= (auto_arr& rhs) throw()
    {
        reset(rhs.release());
        return *this;
    }
    template<class Y>
    auto_arr& operator= (auto_arr<Y>& rhs) throw()
    {
        reset(rhs.release());
        return *this;
    }

    // destructor
    ~auto_arr() throw()
    {
        delete[] ap;
    }

    // value access
    T* get() const throw()
    {
        return ap;
    }

    T& operator*() const throw()
    {
        return *ap;
    }

    T& operator[](int i) const throw()
    {
        return ap[i];
    }

    T* operator->() const throw()
    {
        return ap;
    }

    // release ownership
    T* release() throw()
    {
        T* tmp(ap);
        ap = 0;
        return tmp;
    }

    // reset value
    void reset (T* ptr=0) throw()
    {
        if (ap != ptr)
        {
            delete[] ap;
            ap = ptr;
        }
    }
};

}
