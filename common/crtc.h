#ifndef __crtc_h__
#define __crtc_h__

/* Common class to track screen beam position
 * hPos = current horizontal position
 * hBlank = start of horizontal blank
 * hEnd = end of scanline
 *
 * vPos = current vertical position (scanline)
 * vBlank = start of vertical blank
 * vEnd = last scanline, end-of-frame
 *
 * Tick returns true if at end-of frame
 *  Generates sethblank/setvblank when entering/exiting
 *   hblank and vblank
 */
struct crtc_t {
  int hPos, hBlank, hEnd;
  int vPos, vBlank, vEnd;
  int frame;
  
  virtual bool tick() {
    /* Increase horizontal count */
    if (++hPos == hBlank)
      sethblank(true);
    if (hPos < hEnd)
      return false;
    hPos = 0;
    sethblank(false);
    
    /* Increase vertical count */
    if (++vPos == vBlank)
      setvblank(true);
    if (vPos < vEnd)
      return false;
    vPos = 0;
    setvblank(false);
    
    /* Signal end-of-frame */
    frame++;
    return true;
  };
  virtual void sethblank(bool) { };
  virtual void setvblank(bool) { };

  void init(int hb, int hm, int vb, int vm) {
    if (hm <= hb) {
      hm += hb;
    }
    if (vm <= vb) {
      vm += vb;
    }
    hBlank = hb;
    hEnd = hm;
    vBlank = vb;
    vEnd = vm;
  };
};

#endif
