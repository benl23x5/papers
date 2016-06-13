
#include "colors.inc"
#include "stones.inc"

camera {
    location <0, 300, 600>
    look_at  <0, 0,  0>
  }



sphere 
{   <-200, 0, 0>, 100
    pigment { color rgb <0.4,0.4,1.0> }
    finish {
      ambient .1
      diffuse .7
      reflection .3
    }
}

sphere 
{   <200, 0, 0>, 100
    pigment { color rgb <0.4,0.4,1.0> }
    finish {
      ambient .1
      diffuse .7
      reflection .3
    }
}


sphere 
{   <0, 150, 100>, 50
    pigment { color rgb <1,1,1> }
    finish {
      ambient .1
      diffuse .2
      reflection .8
    }
}

box
{
    <-10000, -100, -10000>, <10000, -100, 10000>
    pigment { checker color rgb<1,1,1>, color rgb<0.4,0.4,0.4> scale 100 }
    finish {
      ambient .2
      diffuse .6
      reflection .2
    }
}

sphere 
{   <0, -80, 0>, 20
    pigment { color rgb <1.0,0.3,1.0> }
    finish {
      ambient .2
      diffuse .6
      reflection .4
    }
}


light_source { <-600, 600, 200> color White}
