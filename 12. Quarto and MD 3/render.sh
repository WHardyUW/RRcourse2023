for i in {1..8}
do
  quarto render Assignment.qmd -P season:$i --output "Assignment_Season_$i.html"
done
