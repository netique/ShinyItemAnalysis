import svg_stack as ss
import os

os.chdir("~/Documents/GitHub/ShinyItemAnalysis-Private/sampleRcode/IRRanimation/irr1_svg")
os.getcwd()

lower = os.listdir("lower")
upper = os.listdir("upper")

for i in range(len(lower)):
  doc = ss.Document()
  lay = ss.VBoxLayout()
  lay.addSVG("upper/" + upper[i])
  lay.addSVG("lower/" + lower[i])
  doc.setLayout(lay)
  doc.save("combined/irr1-" + str(i + 1) + ".svg")
