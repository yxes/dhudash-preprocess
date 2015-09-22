open (H, "headlines.txt") or die "can't open: $!";
   while (<H>) {
	 chomp;
	 my($filename, $headline) = split /:/, $_;
	 $headline =~ s/,.*//;
	 $headline =~ s/"//g;
	 print $filename, "\t", $headline, "\n";
   }
