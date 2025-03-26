import streamlit as st
from pages import intro, simple_regression, multiple_regression, case_studies

# 设置页面配置
st.set_page_config(
    page_title="第六周：回归分析初步",
    page_icon="📊",
    layout="wide"
)

# 应用样式
with open("utils/style.css") as f:
    st.markdown(f"<style>{f.read()}</style>", unsafe_allow_html=True)

# 应用标题
st.title("第六周：回归分析初步")

# 侧边栏导航
st.sidebar.title("导航")
pages = {
    "回归分析概述": intro,
    "简单线性回归模型": simple_regression,
    "多元线性回归模型": multiple_regression,
    "回归分析的实际应用案例": case_studies
}

selection = st.sidebar.radio("选择章节", list(pages.keys()))

# 显示选定页面的内容
pages[selection].app()

# 底部信息
st.sidebar.markdown("---")
st.sidebar.info(
    "本交互式课件由统计学课程组开发，基于Streamlit框架实现。"
) 