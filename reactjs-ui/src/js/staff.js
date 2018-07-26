'use strict';

class StaffFilter extends React.Component {
    render () {
        return (
            <div>
                <label htmlFor="staffFilter">Поиск:</label> 
                <input id="staffFilter" placeholder="Строка поиска" onChange={e => this.props.onChange(e.target.value)}/>
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
                                <tr key={s._peopleId.toString()}>
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

        this.fetchStaff = this.fetchStaff.bind(this);
    }

    componentDidMount() {
        this.fetchStaff(null);
    }

    fetchStaff(q) {
        let url = 'http://localhost:8081/people' + (q == null ? '' : '?q='+q);

        fetch(url)
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
                <StaffFilter onChange={this.fetchStaff}/> 
                <StaffTable staff={this.state.staff}/>
            </div>
        );
    }
}

ReactDOM.render (
    <StaffPage/>,
    document.getElementById('container')
);
