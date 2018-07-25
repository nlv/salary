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
                        <th>Имя</th>
                        <th>Фамилия</th>
                        <th>Отчество</th>
                    </tr>
                </thead>
                <tbody>
                    {
                        this.props.staff.map((s) => {
                            return (
                                <tr>
                                    <td>{s.firstName}</td>
                                    <td>{s.surName}</td>
                                    <td>{s.lastName}</td>
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
    render () {
        return (
            <div>
                <StaffFilter/>
                <StaffTable staff={this.props.staff}/>
            </div>
        );
    }
}

const STAFF = [
    {firstName: 'Петр', surName: 'Петрович', lastName: 'Петров'},
    {firstName: 'Иван', surName: 'Иванович', lastName: 'Иванов'},
    {firstName: 'Сидор', surName: 'Сидорович', lastName: 'Сидоров'}
];

ReactDOM.render (
    <StaffPage staff={STAFF}/>,
    document.getElementById('container')
);
