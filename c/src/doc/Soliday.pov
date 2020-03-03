#include "colors.inc"

global_settings { max_trace_level 255 }

#declare Dark_Crystal = texture {
  pigment {
    color rgbf < 0.2,0.2,1,1 >
  }
  finish {
    specular 0.5
    roughness 0.07
  }
}

#declare Light_Crystal = texture {
  pigment {
    color rgbf < 1,1,1,0.2 >
  }
  finish {
    ambient 0.4
    specular 0.5
    roughness 0.07
  }
}

union {
merge {
  difference {
    merge {
      box { <10,0,-5>,<0,22,5> }
      cylinder { <10,11,-5>,<10,11,5>,11 }
      }
    cylinder { <0,10,-6>,<0,10,6>,10 }
  }
  difference {
    merge {
      box { <10,-20,-5>,<0,0,5> }
      cylinder { <0,-10,-5>,<0,-10,5>,10 }
      }
    cylinder { <10,-10,-6>,<10,-10,6>,10 }
  }
  box { <0,20,-5>,<-21,22,5> }
  texture { Dark_Crystal }
}

merge {
  difference {
    merge {
      box { <0,0,-4>,<-10,20,4> }
      cylinder { <0,10,-4>,<0,10,4>,10 }
      }
    cylinder { <-10,10,-6>,<-10,10,6>,10 }
  }
  difference {
    merge {
      box { <0,-20,-4>,<-10,0,4> }
      cylinder { <-10,-10,-4>,<-10,-10,4>,10 }
      }
    cylinder { <0,-10,-6>,<0,-10,6>,10 }
  }
  texture { Light_Crystal }
  }

merge {
  difference {
    merge {
      box { <-10,0,-5>,<-12,20,5> }
      cylinder { <-10,10,-5>,<-10,10,5>,10 }
      }
    union {
      cylinder { <-12,10,-6>,<-12,10,6>,10 }
      box { <-12,0,-6>,<-22,20,6> }
      }
  }
  difference {
    merge {
      box { <-10,0,-5>,<-12,-20,5> }
      cylinder { <-12,-11,-5>,<-12,-11,5>,11 }
      }
    union {
      cylinder { <-10,-10,-6>,<-10,-10,6>,10 }
      box { <0,-20,-6>,<-10,0,6> }
      }
  }
  box { <21,-22,-5>,<-10,-20,5> }
  texture { Dark_Crystal }
  }

rotate <0,-45,0>
rotate <20,0,0>
}
sky_sphere {
    pigment { color rgb <1,1,1> }
}

camera {
  location  < 0, 0, 150 >
  up <0,1.4,0>
  right <1,0,0>
  look_at   < 1, -1.5, 0 >
  angle 15
}

