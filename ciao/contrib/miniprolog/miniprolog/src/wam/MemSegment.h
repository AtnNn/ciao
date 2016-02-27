/*
** MemSegment.h
** 
** Made by Edison Mera
** Login   <edison@vaioedison>
** 
** Started on  Wed Nov 14 15:07:45 2007 Edison Mera
** Last update Wed Nov 14 15:07:45 2007 Edison Mera
*/

#ifndef   	MEMSEGMENT_H_
# define   	MEMSEGMENT_H_

namespace wam {
  class MemSegment {
    void *memory;
    size_t top;
    size_t lastSize;
  public:
  MemSegment(void *memory) : memory(memory), top(0), lastSize(0) {};
    
    size_t getTop() const { return top; };
    size_t getLastSize() const { return lastSize; };
    void * getTopAddress() const { return (char *)memory + top; };
    void * getAddress(size_t index) const { return (char *)memory + index; };
    size_t getIndex(void *address) const {
      return (char *)address - (char *)memory;
    };
    void incTop(size_t size) { top += size; }
    void decTop(size_t size) { top -= size; }
    void setTop(size_t aTop) { top = aTop; }
    void setLastSize(size_t aLastSize) { lastSize = aLastSize; };
    void push(size_t size) {
      incTop(size);
      setLastSize(size);
    }
    void pop(void) {
      decTop(lastSize);
    }
    void clear() { top = 0; };
  };
};

inline void * operator new(std::size_t s, wam::MemSegment &__p) throw()
{
  void *__q = __p.getTopAddress();
  __p.push(s);
  return __q;
}

inline void * operator new[](std::size_t s, wam::MemSegment &__p) throw()
{
  void *__q = __p.getTopAddress();
  __p.push(s);
  return __q;
}

inline void operator delete (void *__p, wam::MemSegment &__q) throw()
{
  __q.pop();
}

inline void operator delete[] (void *__p, wam::MemSegment &__q) throw()
{
  __q.pop();
}

#endif 	    /* !MEMSEGMENT_H_ */
