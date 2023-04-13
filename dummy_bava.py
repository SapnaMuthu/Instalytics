# import instaloader
# import time

# # Create a loader object
# loader = instaloader.Instaloader(request_timeout=10)

# # Retrieve the profile's data
# profile = instaloader.Profile.from_username(loader.context, 'gvsucab')

# # Print the profile's username, biography, follower count, and following count
# print('Username:', profile.username)
# print('Biography:', profile.biography)
# print('Followers:', profile.followers)
# print('Following:', profile.followees)

# # Iterate over the profile's posts and print each post's number of likes and comments
# for post in profile.get_posts():
#     while True:
#         try:
#             print('Likes:', post.likes)
#             print('Comments:', post.comments)
#             print('Post information:', post)
#             break
#         except instaloader.exceptions.RateLimitException as e:
#             print(f"Got 429 Error. Sleeping for {e.seconds_remaining}s")
#             time.sleep(e.seconds_remaining + 1)


# import instaloader
import pandas as pd
import pytz
# import time
import openpyxl
import sys



def main(input_value):
    # Create a loader object
    # loader = instaloader.Instaloader()
    # print('Username:', input_value)
    # print('Biography:', "laugh louder")
    # print('Followers:', 123)
    # print('Following:', 125)
    # profile_info = [input_value,"laugh louder",123,123]
    # post_info = {'likes_list': [40],
    #              'Comments_list': [50],
    #              'Captions_list': ["I am a kuthu dancer #poda venna"],
    #              'hastags_list': ["#Naevaloperiyapudungi"],
    #              'date_list': ['2023-04-06 14:00:00'],
    #              'url_list': ["https://images.unsplash.com/photo-1552728089-57bdde30beb3?ixlib=rb-4.0.3&ixid=MnwxMjA3fDB8MHxzZWFyY2h8Mnx8cGFycm90fGVufDB8fDB8fA%3D%3D&w=1000&q=80"]
    #             }
    # post_df = pd.DataFrame(post_info)
    # profile_df = pd.DataFrame(profile_info)
    # Create a new DataFrame with timezone-unaware datetime values
    if input_value == "imsanjusamson":         
        post_df = pd.read_csv("Post_info1.csv")
        profile_df = pd.read_csv("Prof_info1.csv")
        #post_df_no_tz = post_df.copy()

        # Write the DataFrame to an Excel file
        # post_df_no_tz.to_excel('gvsucabpost.xlsx', index=False)
        # profile_df.to_excel('gvsucabprof.xlsx',index=False)


        # Write the Dataframe to csv file 
        post_df.to_csv('Post_info.csv', index=False)
        profile_df.to_csv('Prof_info.csv',index=False)
        res = "valid username"
        return (input_value,res)
    else:
        res = "not a valid username"
        return (input_value,res)

if __name__ == '__main__':
    # Get input value from command line arguments
    if len(sys.argv) == 2:
        input_value = sys.argv[1]
        #res = sys.argv[2]
        main(input_value)



# def main(input_value):
#     # Create a loader object
#     # loader = instaloader.Instaloader()

#     print(input_value)


# # Retrieve the profile's datagvsu
#     # profile = instaloader.Profile.from_username(loader.context, input_value)

#     # # Print the profile's username, biography, follower count, and following count
#     # print('Username:', profile.username)
#     # print('Biography:', profile.biography)
#     # print('Followers:', profile.followers)
#     # print('Following:', profile.followees)

# # # Create an empty list to store the post information
# # post_info = []
# # profile_info = [profile.username,profile.biography,profile.followers,profile.followees]

# # # Iterate over the profile's posts and append each post's information to the list
# # for post in profile.get_posts():
# #     while True:
# #         try:
# #             post_data = {
# #                 'likes_list': post.likes,
# #                 'Comments_list': post.comments,
# #                 'Caption_list': post.caption,
# #                 'hashtags_list': post.caption_hashtags,
# #                 'date_list': post.date_local,
# #                 #'Post URL': f"https://www.instagram.com/p/{post.shortcode}/"
# #                 'url_list': post.url
# #             }
# #             post_info.append(post_data)
# #             break
# #         except instaloader.exceptions.TooManyRequestsException as e:
# #             print(f"Got 429 Error. Sleeping for {e.retry_after} seconds")
# #             time.sleep(e.retry_after + 1)   
# # # Convert the post information list to a pandas DataFrame
# # # profile_info.append(profile.username,profile.biography,profile.followers,profile.followees)
# # # profile_df = pd.DataFrame(profile_info)
# # post_df = pd.DataFrame(post_info)
# # profile_df = pd.DataFrame(profile_info)
# # # Create a new DataFrame with timezone-unaware datetime values
# # post_df_no_tz = post_df.copy()
# # post_df_no_tz['date_list'] = post_df_no_tz['date_list'].apply(lambda x: x.astimezone(pytz.utc).replace(tzinfo=None))

# # # Write the DataFrame to an Excel file
# # # post_df_no_tz.to_excel('gvsucabpost.xlsx', index=False)
# # # profile_df.to_excel('gvsucabprof.xlsx',index=False)


# # # Write the Dataframe to csv file 
# # post_df_no_tz.to_csv('Post_info.csv', index=False)
# # profile_df.to_csv('Prof_info.csv',index=False)


# # print(post_df_no_tz)
# # print(profile_df)


# if __name__ == '__main__':
#     input_value = sys.argv[1]
#     main(input_value)
