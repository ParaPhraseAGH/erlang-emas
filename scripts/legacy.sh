#!/usr/bin/env sh

for f in `ls $1`; do
	# echo ""$f
	for ff in `ls $1/$f`; do
		# echo "  "$ff
		for fff in `ls $1/$f/$ff`; do
			# echo "    "$fff
			awk '{print $1 F $2}' F=' <leg> ' $1/$f/$ff/$fff > tmp && mv tmp $1/$f/$ff/$fff
		done
	done
done