import pyautogui
import time
from selenium import webdriver
from selenium.webdriver.common.action_chains import ActionChains

driver = webdriver.Chrome("/home/filip/Install/chromedriver") 
driver.get("https://shiny.cs.cas.cz/ShinyItemAnalysisTestPM/")
input("Press enter to continue")
slider = driver.find_element_by_css_selector("#tab-5461-4 > div.row > div:nth-child(1) > div > span > span.irs-slider.single")
time.sleep(1)

for i in range(0, 97):
    move = ActionChains(driver)
    move.click_and_hold(slider).move_by_offset(-1, 0).release().perform()
    time.sleep(5)
    screenShot = pyautogui.screenshot()
    screenShot.save(f"/home/filip/Documents/screnshot{i}.png")