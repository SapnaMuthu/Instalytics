import instaloader
import pandas as pd
import pytz
import time
import openpyxl
import sys

def main(input_value):
        
    # Create a loader object
    loader = instaloader.Instaloader()
    loader.context.no_metadata_json = True
    loader.context._rate_controller.enbaled = True

    try:
        # Retrieve the profile's data
            # Create a loader object
        profile = instaloader.Profile.from_username(loader.context, input_value)
        res = "valid username"
        # Print the profile's username, biography, follower count, and following count
        print('Username:', profile.username)
        print('Biography:', profile.biography)
        print('Followers:', profile.followers)
        print('Following:', profile.followees)

        # Create an empty list to store the post information
        post_info = []
        profile_info = [profile.username,profile.biography,profile.followers,profile.followees]
        count = 0
        # Iterate over the profile's posts and append each post's information to the list
        received = profile.get_posts()
        loader.close()
        for post in received:
            if count % 70 == 0:
                time.sleep(3)
            while True:
                try:
                    # time.sleep(1.6)
                    post_data = {
                        'likes_list': post.likes,
                        'Comments_list': post.comments,
                        'Caption_list': post.caption,
                        'hashtags_list': post.caption_hashtags,
                        'date_list': post.date_local,
                        #'Post URL': f"https://www.instagram.com/p/{post.shortcode}/"
                        'url_list': post.url
                    }
                    post_info.append(post_data)
                    break
                except instaloader.exceptions.TooManyRequestsException as e:
                    print(f"Got 429 Error. Sleeping for {e.retry_after} seconds")
                    time.sleep(e.retry_after + 1)   
                    return(input_value,str(e))
                except Exception as e:
                    print(f"An error occurred while processing the post. Error message: {str(e)}")
                    return(input_value,str(e))
        # Convert the post information list to a pandas DataFrame
        post_df = pd.DataFrame(post_info)
        profile_df = pd.DataFrame(profile_info)
        # Create a new DataFrame with timezone-unaware datetime values
        post_df_no_tz = post_df.copy()
        post_df_no_tz['date_list'] = post_df_no_tz['date_list'].apply(lambda x: x.astimezone(pytz.utc).replace(tzinfo=None))

        # Write the Dataframe to csv file 
        post_df_no_tz.to_csv('Post_info.csv', index=False)
        profile_df.to_csv('Prof_info.csv',index=False)

        return (input_value, res)

    except instaloader.exceptions.ProfileNotExistsException:
        res = "not a valid username"
        return (input_value, res)
    except Exception as e:
        print(f"An error occurred while processing the profile. Error message: {str(e)}")
        return (input_value, "error")    


if __name__ == '__main__':
    # Get input value from command line arguments
    if len(sys.argv) == 2:
        input_value = sys.argv[1]
        #res = sys.argv[2]
        main(input_value)
