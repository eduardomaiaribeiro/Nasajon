#!/usr/bin/env python3
"""
Example script demonstrating how to use the Task Management API
"""

import requests
import json

BASE_URL = "http://localhost:5000/api"


def print_response(response, action):
    """Helper function to print API responses"""
    print(f"\n{'='*60}")
    print(f"ACTION: {action}")
    print(f"STATUS CODE: {response.status_code}")
    print(f"RESPONSE:")
    print(json.dumps(response.json(), indent=2))
    print('='*60)


def main():
    print("Task Management API - Example Usage")
    print("====================================")
    
    # 1. Health Check
    response = requests.get(f"{BASE_URL}/health")
    print_response(response, "Health Check")
    
    # 2. Create a new task
    task_data = {
        "title": "Complete project documentation",
        "description": "Write comprehensive README and API documentation",
        "status": "in_progress"
    }
    response = requests.post(f"{BASE_URL}/tasks", json=task_data)
    print_response(response, "Create Task")
    
    if response.status_code != 201 or not response.json().get('success'):
        print("\n❌ ERROR: Failed to create task")
        return
    
    task_id = response.json()['data']['id']
    
    # 3. Create another task
    task_data = {
        "title": "Implement unit tests",
        "description": "Write tests for all API endpoints",
        "status": "pending"
    }
    response = requests.post(f"{BASE_URL}/tasks", json=task_data)
    print_response(response, "Create Another Task")
    
    # 4. Get all tasks
    response = requests.get(f"{BASE_URL}/tasks")
    print_response(response, "Get All Tasks")
    
    # 5. Get specific task by ID
    response = requests.get(f"{BASE_URL}/tasks/{task_id}")
    print_response(response, f"Get Task by ID: {task_id}")
    
    # 6. Update task
    update_data = {
        "status": "completed",
        "description": "Documentation completed successfully!"
    }
    response = requests.put(f"{BASE_URL}/tasks/{task_id}", json=update_data)
    print_response(response, f"Update Task: {task_id}")
    
    # 7. Delete task
    response = requests.delete(f"{BASE_URL}/tasks/{task_id}")
    print_response(response, f"Delete Task: {task_id}")
    
    # 8. Verify deletion - try to get the deleted task
    response = requests.get(f"{BASE_URL}/tasks/{task_id}")
    print_response(response, "Verify Deletion (Should be 404)")
    
    # 9. Get all remaining tasks
    response = requests.get(f"{BASE_URL}/tasks")
    print_response(response, "Get All Remaining Tasks")


if __name__ == "__main__":
    print("\nMake sure the API server is running:")
    print("  python app.py\n")
    
    try:
        main()
    except requests.exceptions.ConnectionError:
        print("\n❌ ERROR: Could not connect to the API server.")
        print("Please make sure the server is running on http://localhost:5000")
        print("Start the server with: python app.py")
    except Exception as e:
        print(f"\n❌ ERROR: {str(e)}")
