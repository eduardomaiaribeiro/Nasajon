"""
Tests for the Task Management API
"""

import pytest
import json
from app import app, tasks


@pytest.fixture
def client():
    """Create a test client"""
    app.config['TESTING'] = True
    with app.test_client() as client:
        yield client
    # Clear tasks after each test
    tasks.clear()


def test_health_check(client):
    """Test the health check endpoint"""
    response = client.get('/api/health')
    assert response.status_code == 200
    data = json.loads(response.data)
    assert data['success'] is True
    assert data['status'] == 'healthy'


def test_get_empty_tasks(client):
    """Test getting tasks when none exist"""
    response = client.get('/api/tasks')
    assert response.status_code == 200
    data = json.loads(response.data)
    assert data['success'] is True
    assert data['count'] == 0
    assert len(data['data']) == 0


def test_create_task(client):
    """Test creating a new task"""
    task_data = {
        'title': 'Test Task',
        'description': 'This is a test task',
        'status': 'pending'
    }
    response = client.post('/api/tasks',
                          data=json.dumps(task_data),
                          content_type='application/json')
    assert response.status_code == 201
    data = json.loads(response.data)
    assert data['success'] is True
    assert data['data']['title'] == 'Test Task'
    assert data['data']['description'] == 'This is a test task'
    assert 'id' in data['data']


def test_create_task_without_title(client):
    """Test creating a task without a title (should fail)"""
    task_data = {
        'description': 'This is a test task'
    }
    response = client.post('/api/tasks',
                          data=json.dumps(task_data),
                          content_type='application/json')
    assert response.status_code == 400
    data = json.loads(response.data)
    assert data['success'] is False


def test_get_task_by_id(client):
    """Test getting a specific task by ID"""
    # First create a task
    task_data = {
        'title': 'Test Task',
        'description': 'This is a test task'
    }
    create_response = client.post('/api/tasks',
                                 data=json.dumps(task_data),
                                 content_type='application/json')
    created_task = json.loads(create_response.data)['data']
    task_id = created_task['id']
    
    # Now get it
    response = client.get(f'/api/tasks/{task_id}')
    assert response.status_code == 200
    data = json.loads(response.data)
    assert data['success'] is True
    assert data['data']['id'] == task_id


def test_get_nonexistent_task(client):
    """Test getting a task that doesn't exist"""
    response = client.get('/api/tasks/nonexistent-id')
    assert response.status_code == 404
    data = json.loads(response.data)
    assert data['success'] is False


def test_update_task(client):
    """Test updating a task"""
    # First create a task
    task_data = {
        'title': 'Original Title',
        'description': 'Original Description'
    }
    create_response = client.post('/api/tasks',
                                 data=json.dumps(task_data),
                                 content_type='application/json')
    created_task = json.loads(create_response.data)['data']
    task_id = created_task['id']
    
    # Update it
    update_data = {
        'title': 'Updated Title',
        'status': 'completed'
    }
    response = client.put(f'/api/tasks/{task_id}',
                         data=json.dumps(update_data),
                         content_type='application/json')
    assert response.status_code == 200
    data = json.loads(response.data)
    assert data['success'] is True
    assert data['data']['title'] == 'Updated Title'
    assert data['data']['status'] == 'completed'


def test_update_task_with_empty_body(client):
    """Test updating a task with empty JSON body"""
    # First create a task
    task_data = {
        'title': 'Test Task',
        'description': 'Test Description'
    }
    create_response = client.post('/api/tasks',
                                 data=json.dumps(task_data),
                                 content_type='application/json')
    created_task = json.loads(create_response.data)['data']
    task_id = created_task['id']
    
    # Try to update with empty JSON object (valid but no changes)
    response = client.put(f'/api/tasks/{task_id}',
                         data=json.dumps({}),
                         content_type='application/json')
    # Should succeed with 200 (no error, just no changes)
    assert response.status_code == 200
    data = json.loads(response.data)
    assert data['success'] is True


def test_delete_task(client):
    """Test deleting a task"""
    # First create a task
    task_data = {
        'title': 'Task to Delete',
        'description': 'This task will be deleted'
    }
    create_response = client.post('/api/tasks',
                                 data=json.dumps(task_data),
                                 content_type='application/json')
    created_task = json.loads(create_response.data)['data']
    task_id = created_task['id']
    
    # Delete it
    response = client.delete(f'/api/tasks/{task_id}')
    assert response.status_code == 200
    data = json.loads(response.data)
    assert data['success'] is True
    
    # Verify it's gone
    get_response = client.get(f'/api/tasks/{task_id}')
    assert get_response.status_code == 404


def test_delete_nonexistent_task(client):
    """Test deleting a task that doesn't exist"""
    response = client.delete('/api/tasks/nonexistent-id')
    assert response.status_code == 404
    data = json.loads(response.data)
    assert data['success'] is False
