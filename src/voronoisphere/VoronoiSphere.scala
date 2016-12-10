package voronoisphere

import java.util.Random
import geom.V3
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.io.File
import geom.V2

object VoronoiSphere {
  
  def main(args: Array[String]) = {
    val rand = new Random(1248115)
    val nPts = 128
    var pts = Vector.fill(nPts)(V3(rand.nextGaussian(), rand.nextGaussian(), rand.nextGaussian()).unit)
    for (i <- 0 until 500) {
      val near = pts.map(p => p -> (pts.sortBy(-p * _).drop(1).view)).toMap
      def nearest(p: V3) = {
        pts.maxBy { v => p * v }
      }
      val sz = 1024
      val img = new BufferedImage(sz, sz, BufferedImage.TYPE_INT_ARGB)
      for (xP <- 0 until sz) {
        for (yP <- 0 until sz) {
          val x = xP.toDouble / sz * 2 - 1.0
          val y = yP.toDouble / sz * 2 - 1.0
          val d = math.hypot(x, y)
          if (d > 1.0) {
            img.setRGB(xP, yP, 0xFF000000)
          }
          else {
            val z = math.sqrt(1 - d)
            val v = V3(x, y, z)
            val t = pts.maxBy { t => t * v }
            val col = t.hashCode()
            val r = ((col >> 16) & 0xFF) / 255.0
            val g = ((col >>  8) & 0xFF) / 255.0
            val b = ((col >>  0) & 0xFF) / 255.0
            val l = (V3(x, y, z) * V3(-0.7, -0.7, 1).unit + 1.0) / 2.0
            val rl = r * l
            val gl = g * l
            val bl = b * l
            val rc = ((rl * 255).toInt max 0 min 255) << 16
            val gc = ((gl * 255).toInt max 0 min 255) <<  8
            val bc = ((bl * 255).toInt max 0 min 255) <<  0
            img.setRGB(xP, yP, 0xFF000000 | rc | gc | bc)
          }
        }
      }
      ImageIO.write(img, "png", new File(s"res$i.png"))
      println(i)
      def corner(p1: V3, p2: V3, p3: V3) = {
        val a = (p2 - p1).unit
        val z = (a cp (p3 - p1)).unit
        val b = (z cp a).unit
        val r2 = p2 - p1
        val r3 = p3 - p1
        val pr2 = V2(a * r2, b * r2)
        val pr3 = V2(a * r3, b * r3)
        val p = pr2 / 2
        val q = pr3 / 2
        val pr = -p.cross
        val qr = q.cross
        val alpha = ((p - q) cp pr) / (qr cp pr)
        val c2 = q + qr * alpha
        val c = a * c2.x + b * c2.y
        c + p1
      }
      def touch(p1: V3, p2: V3, p3: V3) = {
        val c = corner(p1, p2, p3)
        val p = nearest(c)
        p == p1 || p == p2 || p == p3
      }
      val tiles = pts.map(p => p -> {
        val n = near(p)(0)
        var ns = Vector[V3]()
        while (ns.lastOption != Some(n)) {
          val o = ns.lastOption.getOrElse(n)
          val it = near(p).find { nx =>
            if (nx == o || ((p - nx) cp (p - o)) * p < 0) false
            else {
              val c = corner(p, o, nx)
              val n = (nearest(c) - c).norm
              val err = V3(
                  n - (p - c).norm,
                  n - (o - c).norm,
                  n - (nx - c).norm).norm2
              err < 0.0000000000001
            }
          }.get
          ns :+= it
        }
        val neighs = ns.zip(ns.tail :+ ns.head)
        val corners = neighs.map { case (p1, p2) => corner(p, p1, p2) }
        corners
      }).toMap
      pts = pts.map { p =>
        val tile = tiles(p)
        tile.reduce(_ + _).unit
      }
    }
  }
  
}