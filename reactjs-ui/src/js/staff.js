'use strict';

class StaffFilter extends React.Component {
    render () {
        return (
            <div>
                <label htmlFor="staffFilter">Поиск:</label> 
                <input id="staffFilter" placehoder="Иванов"/>
            </div>
        );
    }
}


class StaffTable extends React.Component {
    render () {
        return (
            <table>
                <thead>
                    <tr>
                        <th>Фамилия</th>
                        <th>Имя</th>
                        <th>Отчество</th>
                    </tr>
                </thead>
                <tbody>
                    {
                        this.props.staff.map((s) => {
                            return (
                                <tr>
                                    <td>{s._peopleLastName}</td>
                                    <td>{s._peopleFirstName}</td>
                                    <td>{s._peopleSurName}</td>
                                </tr>
                            );
                        })
                    }
                </tbody>
            </table>
        );
    }
}

class StaffPage extends React.Component {
       
    constructor(props) {
        super(props);
        this.state = { staff: [] };
    }

    componentDidMount() {
        fetch('http://localhost:8081/people', {mode: 'no-cors'})
            .then(response => {
                if (response.ok) {
                    return response.json();
                } else {
                    throw new Error('Something went wrong ...');
                }
            })
            .then(data => { this.setState({ staff: data }) })
            .catch(err => alert (err))
        ;
    }

    render () {
        return (
            <div>
                <StaffFilter/>
                <StaffTable staff={this.state.staff}/>
            </div>
        );
    }
}

ReactDOM.render (
    <StaffPage/>,
    document.getElementById('container')
);
