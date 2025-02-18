#ifndef __gte_h__
#define __gte_h__

template<typename T, int N>struct vec {
  T v[N] = { 0 };
  const T& operator[](int x) const {
    return v[x];
  };
  T& operator[](int x) {
    return v[x];
  };
  vec<T, N>operator*(const vec<T,N>& rhs) {
    vec<T, N> r;

    for (int i = 0; i < N; i++)
      r.v[i] = v[i] * rhs.v[i];
    return r;
  };
  vec<T, N>operator+(const vec<T,N>& rhs) {
    vec<T, N> r;

    for (int i = 0; i < N; i++)
      r.v[i] = v[i] + rhs.v[i];
    return r;
  };
  vec<T, N>operator-(const vec<T,N>& rhs) {
    vec<T, N> r;

    for (int i = 0; i < N; i++)
      r.v[i] = v[i] - rhs.v[i];
    return r;
  };
};

template<typename T, int N, int M>struct mtx {
  vec<T, N> row[M];
  const vec<T, N>& operator[](int r) const {
    return row[r];
  };
  vec<T, N>& operator[](int r) {
    return row[r];
  };

  /* Matrix x Vector */
  vec<T, N> operator*(const vec<T, N>& rhs) {
    vec<T, N> r;

    for (int i = 0; i < M; i++)
      r = (row[i] * rhs) + r;
    return r;
  };
};

typedef vec<int16_t, 3>     vec3s16;
typedef vec<int16_t, 2>     vec2s16;
typedef vec<int32_t, 3>     vec3s32;
typedef mtx<int16_t, 3, 3>  mtx3x3;

/* Rectangle */
typedef vec<int16_t, 4>     rect;

static void showvec(vec3s16& v, const char *lbl) {
  printf("Vec3[%s] %.4x %.4x %.4x\n", lbl, v[0], v[1], v[2]);
}

static void showmtx(mtx3x3& m, const char *lbl) {
  printf("Mtx3x3[%s]\n", lbl);
  printf("%.4x %.4x %.4x\n%.4x %.4x %.4x\n%.4x %.4x %.4x\n",
	 m[0][0], m[0][1], m[0][2],
	 m[1][0], m[1][1], m[1][2],
	 m[2][0], m[2][1], m[2][2]);
}

#endif
