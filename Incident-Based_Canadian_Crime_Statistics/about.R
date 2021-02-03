function(){
  tabPanel("About",
           HTML("<h1> Andrea Payne</h1>
        <p> This data set details incident-based crime statistics nationwide by type of violation. Visualization and analysis of this data provides an outlook into developing trends within Canadian Criminal Code Violations. This is of specfic interest as it allows for targeted outreach programs to be developed in order to reduce future violations. This dashboard uses single linear regression with the total population to predict the values along with a 95% confidence interval.</p>
		    <p>  Data Source:  Statistics Canada. <a href='https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=3510017701'>Table 35-10-0177-01  Incident-based crime statistics, by detailed violations, Canada, provinces, territories and Census Metropolitan Areas</a> (<a href='https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=3510017701'>DOI</a>). Retrieved on January 31, 2021. </p>
        <p> About the Author: I am a fourth year student at Carleton University in Ottawa, ON. I am currently working towards a Bachelor of Mathematics in Computational and Applied Mathematics and Statisics, with concentration in Applied Statistics and Probablility. For other instances of my work, please view my GitHub linked below.</p>"
           ),
           HTML('
        <div style="clear: left;">
        <img src="https://media-exp1.licdn.com/dms/image/C4E03AQGIfNl3eAQOPg/profile-displayphoto-shrink_800_800/0/1611601287081?e=1617840000&v=beta&t=fiUnCbn62Ph3MIN49fdogWSFVVDQ5Mp77eTBT5s-r84" alt="" style="height: 204px; width: 204px; "> </div>
        <p>
        <a href="https://www.linkedin.com/in/andrea-payne-43395a197/" target="_blank">LinkedIn</a>
        <a href="https://github.com/andreapayne29" target="_blank">GitHub</a><br/>
        </p>'),#End of html part 2
           value="about"
  )
}