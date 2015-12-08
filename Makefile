default:
	@echo nothing done

clean:
	rm -f .BAK.* *~

zip:	clean
	cd ..; zip -r ass3 clean_parser_assignment
