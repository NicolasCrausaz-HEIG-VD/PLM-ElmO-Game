import './App.css'
import { Card } from './components/Card'

function App() {
  return (
    <div style={{ display: 'flex', flexWrap: 'wrap' }}>
      <Card type='0_yellow' size="200px" />
      <Card type='1_blue' size="200px" />
      <Card type='wild' size="200px" />
      <Card type='wild_draw_four' size="200px" />
      <Card type='draw_blue' size="200px" />
      <Card type='skip_green' size="200px" />
    </div>
  )
}

export default App
