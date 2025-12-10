"""
Simple Task Management API - Nasajon Hiring Test
A RESTful API for managing tasks with basic CRUD operations.
"""

from flask import Flask, jsonify, request
from datetime import datetime, timezone
import uuid

app = Flask(__name__)

# In-memory storage for tasks
tasks = {}


@app.route('/api/tasks', methods=['GET'])
def get_tasks():
    """Get all tasks"""
    return jsonify({
        'success': True,
        'data': list(tasks.values()),
        'count': len(tasks)
    }), 200


@app.route('/api/tasks/<task_id>', methods=['GET'])
def get_task(task_id):
    """Get a specific task by ID"""
    task = tasks.get(task_id)
    if not task:
        return jsonify({
            'success': False,
            'error': 'Task not found'
        }), 404
    
    return jsonify({
        'success': True,
        'data': task
    }), 200


@app.route('/api/tasks', methods=['POST'])
def create_task():
    """Create a new task"""
    data = request.get_json()
    
    if not data or 'title' not in data:
        return jsonify({
            'success': False,
            'error': 'Title is required'
        }), 400
    
    task_id = str(uuid.uuid4())
    task = {
        'id': task_id,
        'title': data['title'],
        'description': data.get('description', ''),
        'status': data.get('status', 'pending'),
        'created_at': datetime.now(timezone.utc).isoformat(),
        'updated_at': datetime.now(timezone.utc).isoformat()
    }
    
    tasks[task_id] = task
    
    return jsonify({
        'success': True,
        'data': task
    }), 201


@app.route('/api/tasks/<task_id>', methods=['PUT'])
def update_task(task_id):
    """Update an existing task"""
    if task_id not in tasks:
        return jsonify({
            'success': False,
            'error': 'Task not found'
        }), 404
    
    data = request.get_json()
    
    # Check if data is None (invalid JSON or no content-type)
    if data is None:
        return jsonify({
            'success': False,
            'error': 'Invalid JSON data'
        }), 400
    
    task = tasks[task_id]
    
    if 'title' in data:
        task['title'] = data['title']
    if 'description' in data:
        task['description'] = data['description']
    if 'status' in data:
        task['status'] = data['status']
    
    task['updated_at'] = datetime.now(timezone.utc).isoformat()
    
    return jsonify({
        'success': True,
        'data': task
    }), 200


@app.route('/api/tasks/<task_id>', methods=['DELETE'])
def delete_task(task_id):
    """Delete a task"""
    if task_id not in tasks:
        return jsonify({
            'success': False,
            'error': 'Task not found'
        }), 404
    
    deleted_task = tasks.pop(task_id)
    
    return jsonify({
        'success': True,
        'data': deleted_task,
        'message': 'Task deleted successfully'
    }), 200


@app.route('/api/health', methods=['GET'])
def health_check():
    """Health check endpoint"""
    return jsonify({
        'success': True,
        'status': 'healthy',
        'timestamp': datetime.now(timezone.utc).isoformat()
    }), 200


if __name__ == '__main__':
    app.run(debug=True, host='0.0.0.0', port=5000)
