package fr.inria.lille.graphdep.svg

/**
 * Created by Jiraya on 13/12/2014.
 */

abstract class ColorSystem
case class RGB(red : Int, green : Int, blue : Int) extends ColorSystem {
  if(red < 0  || red > 255) {
    throw new IllegalArgumentException("RGB values must be between 0 and 255 (red : "+ red +")")
  }

  if(green < 0  || green > 255) {
    throw new IllegalArgumentException("RGB values must be between 0 and 255 (green : "+ green +")")
  }

  if(blue < 0  || blue > 255) {
    throw new IllegalArgumentException("RGB values must be between 0 and 255 (blue : "+ blue +")")
  }

  override def toString : String = {
    "rgb("+ red +", "+ green +", "+ blue +")"
  }
}
