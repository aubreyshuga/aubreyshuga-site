---
title: Senior Project Proposal and Background
author: Aubrey Shuga
date: '2021-02-18'
slug: senior-project-proposal
categories: ["Senior Project"]
tags: []
subtitle: ''
summary: "For my Senior Project, I would like to perform an exploratory data analysis on student data provided by BYU-Idaho Peer-Success Mentoring. They provide short-term mentoring to a variety of BYU-Idaho student populations with a goal to improve student success and retention. We’re interested in looking for possible correlation between participation in mentoring and defined measures of student success, including improved GPA."
authors: []
lastmod: '2021-02-18T05:34:24-07:00'
featured: no
draft: yes
image:
  caption: ''
  focal_point: ''
  preview_only: no
projects: []
---

# Project Proposal

For my Senior Project, I would like to perform an exploratory data analysis on student data provided by BYU-Idaho Peer-Success Mentoring. They provide short-term mentoring to a variety of BYU-Idaho student populations with a goal to improve student success and retention. We’re interested in looking for possible correlation between participation in mentoring and defined measures of student success, including improved GPA.

This project will be focused on answering the following question: 

**Do students who participate in Peer-Success Mentoring have more success (improvement in GPA) than students who don't participate in Peer-Success Mentoring?**

This question will be explored through various forms of data analysis, hypothesis testing, and inferential statistics, using data about students who met defined mentoring criteria. A Control group of students who met the criteria but did not participate in mentoring will be compared with a Mentored group of students who met the same criteria and did participate in mentoring.

## Expected Tasks and Deliverables

 * Exploratory Data Analysis of student GPAs over 4 semesters - Mentored vs Control [(view)]()
 * Paired Samples t-Test: "Is there an increase from Pre GPA to Post GPA for the average Tier 3 student?" [(view)]()
 * Independent Samples t-Test: "Do students in the Mentored group have a higher increase from Pre GPA to Post GPA than students in the Control group?" [(view)]()
 * Exploratory Data Analysis of different mentee "status" groups
 * Paired Samples t-Test: "Is there an increase from Pre GPA to Post GPA for the average mentee in?" performed for each "status" group
 * Independent Samples t-Test: "Do mentees who fulfilled the mentoring program have a higher increase from Pre GPA to Post GPA than students in the Control group?"
 * Independent Samples t-Test: "Do mentees who fulfilled the mentoring program have a higher increase from Pre GPA to Post GPA than mentees who did not fulfill the mentoring program?"
 * Interactive dashboard to view visuals, summary stats, and test results for each group
 * Final Project Presentation
 * Set up a framework for BYU-I Student Support to continue this type of analysis in future semesters and replicate it on other programs within the department (other mentoring groups, student workshops, etc.)
 * Gather more data to replicate this analysis for more recent mentees

---

# Background

## BYU-Idaho's outreach to struggling students

In 2017, BYU-Idaho adjusted their academic standing policy to allow struggling students to continue attending school and to help provide them with the resources needed to succeed. Previously, students were placed on academic warning, probation, suspension, and eventually dismissal. When this change occurred, over 1,900 previously suspended students were invited to re-enroll in school, and 1,271 of them did.
By the end of 2018, 82% of these returned students were still enrolled in school or had graduated, and 42% had improved their GPA. [source](https://www.byui.edu/newsroom/news-and-notes/archive/3-25-19-struggling-students) 

The goal of this new policy is to intervene at the first sign of struggle and give students the support they need while it is still early. To aid in this effort, a variety of campus resources were created to reach out to these students, including Peer-Success Mentoring.


## Peer-Success Mentoring

The Peer-Success Mentoring program (previously named "At-Risk Mentoring") provides short-term (3 weeks) one-on-one mentoring for students who are considered “at-risk” at the university. Mentors reach out to students who are struggling academically as well as students who are referred to the program from faculty, campus service centers, peers, and self-referrals. The program is designed to assess these students’ needs and get them connected to the appropriate student support and campus resources.

#### How it works

Peer-Success Mentors are student employees who are trained in mentoring, goal-setting, and campus resources. At the start of each semester, the university administration provides the mentoring program with a list of students who meet certain "at-risk" criteria (defined below). The mentors then attempt to contact each student on the list and invite them to participate in mentoring. If a student chooses to enroll as a mentee, they will be assigned to one mentor and expected to meet with their mentor 3-4 times to complete the program. Sometimes, mentees drop out or are referred to other mentoring programs without finishing all of the expected mentoring sessions and "fulfilling" the program.

For most of these mentees, participating in mentoring is optional. Certain students are required to attend a one-time consultation with a Peer-Success Mentor in order to register for classes or to receive credit for a class assignment. After this consultation, these students are then invited to enroll as mentee. None of these students are included in the provided data for this project, so those populations will not be explored for now.

The Peer-Success Mentoring program has undergone some changes since its beginnings in 2018. The data for this project reflects the mentoring work done in Fall 2018 and Winter 2019, and the program has changed since then. Once more recent data is collected, it will be interesting to perform a similar analysis and compare the findings.

---

# Definitions


## Mentoring / "At-Risk" Criteria

Peer-Success Mentoring works with many different student populations who meet various "at-risk" critera. In the provided dataset, there are only two student populations - Tier 3 and Referral.

#### Tier 3

Most students in the provided data are classified as "Tier 3". These students were all offered mentoring and either participated or did not participate, so they are assigned into either the Mentored or Control group.

Students are placed in "tiers" based on their academic performance. Tier 3 can be defined by the following: "The third tier category focuses on freshmen, in particular any freshman with a 2.0 GPA or lower after their second semester. These students are referred to the [Peer-Success] Mentoring program, where they receive a personal student mentor’s help in addition to their academic advisor." [source](https://www.byui.edu/newsroom/news-and-notes/archive/3-25-19-struggling-students)

#### Referral

A smaller number of students in this data are classified as "Referral". These students were referred to Peer-Success Mentoring by a professor, friend, faculty member, or other person, or they signed up for mentoring on their own accord. These students do not necessarily meet the Tier 3 criteria, so the impact of mentoring might be different.

#### Other Populations

Other Peer-Success Mentoring populations (not included in this project) include the following:

 * Freshman enrolled in a required "College Success" course (one-time consultation required as a class assignment)
 
 * New students who indicate on a survey that they are struggling in one or more aspects of student life (invited for a consultation and to enroll in mentoring)
 
 * Tier 1 students (similar to Tier 3 - contacted by a mentor and invited to enroll in optional mentoring)
 
 * Tier 4 (one-time consultation required before they can register for the next semester)
 
 * Mentor Hub Recruitment (students who visit the Mentor Hub and participate in an immediate consultation with a mentor)



## Mentee Status

Every student that meets mentoring criteria is assigned a "status" that describes where the student is at in the mentoring process. Their mentor adjusts this status as the student moves through the process. In this project, each student's status is the status they were marked as at the end of their mentoring semester. The following statuses are included in this project:

#### Attempted

Students classified as "Attempted" are students who met Tier 3 criteria and were contacted by a Peer-Success Mentor but did not participate in mentoring. They could have explicitly declined mentoring or could have not responded to the mentor's contact attempts. These students make up the control group.

#### Fulfilled Program

These students completed the 3-4 mentoring sessions in the program.

#### Dropped

These students were dropped from the mentoring program for a variety of reasons:

 * They attended one or more mentoring sessions and informed the mentor that they wanted to discontinue mentoring.
 
 * They attended one or more mentoring sessions but "no-showed" for their remaining sessions without communicating to the mentor that they wanted to discontinue mentoring
 
 * They enrolled in mentoring but "no-showed" for their first session without communicating to the mentor that they wanted to discontinue mentoring
 
#### Referred to HJG/LS

These students were referred to other peer-mentoring programs, including Heber J. Grant Mentoring and Life-Skills Mentoring. These students could have met with a Peer-Success Mentor one or more times before being referred to another program, or they could have been referred without meeting with a Peer-Success Mentor.

#### Active Mentoring

These students enrolled in Peer-Success Mentoring, but for some reason were never marked as "Fulfilled Program". This could be due to a variety of reasons:

 * The student signed up for mentoring, but never scheduled or attended any of their sessions
 
 * They attended one or more mentoring sessions but then "no-showed" for their remaining sessions without communicating to the mentor that they wanted to discontinue mentoring
 
 * The student actively participated in mentoring, but enrolled too late in the semester to complete enough sessions to be marked as "fulfilled" (Each semester, the mentors start over with a new list of Tier 3 students)
 
 * The student was moved to another status (Fulfilled, Dropped, Referred to HJG/LS), but the mentor forgot to change the student's status in the records.
 
Because of this, it is hard to know the experience that these students had. They probably do not all belong in the same group.




