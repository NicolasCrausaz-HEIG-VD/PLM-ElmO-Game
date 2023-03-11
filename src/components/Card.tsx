import style from './Card.module.css'

type num = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
type color = "red" | "blue" | "green" | "yellow"

type CardType = "wild" | "wild_draw_four" | `${num}_${color}` | `draw_${color}` | `skip_${color}` | `reverse_${color}`

type CardProps = {
  type: CardType
  size?: string
}

export const Card = ({ type, size="auto" }: CardProps) => {
  const src = `/cards/${type}.svg`
  return (
    <div className={style.card}>
      <div className={style.card_inner}>
        <img src={src} alt={type} className={style.card_front} height={size} />
        <img src="/cards/empty.svg" alt="back of card" className={style.card_back} />
      </div>
    </div>
  )
}
