-- Seed: 10926155887209372240,13903879141658024201



entity ulydeda is
  port (niwa : buffer boolean_vector(1 to 4));
end ulydeda;



architecture szimbama of ulydeda is
  
begin
  
end szimbama;



entity j is
  port (ht : in integer_vector(2 to 4); hypb : out time; bggmjutshi : out time);
end j;



architecture s of j is
  signal yiycbguxca : boolean_vector(1 to 4);
begin
  oy : entity work.ulydeda
    port map (niwa => yiycbguxca);
end s;



entity kpqbrlhbaa is
  port (zmpakcrzzl : buffer real; wstowgyd : inout severity_level; tlyfg : linkage integer);
end kpqbrlhbaa;



architecture fao of kpqbrlhbaa is
  signal ukdkcqdqf : boolean_vector(1 to 4);
  signal qjhisv : boolean_vector(1 to 4);
  signal kerm : time;
  signal vguhva : time;
  signal tmltzo : integer_vector(2 to 4);
begin
  rsxfxz : entity work.j
    port map (ht => tmltzo, hypb => vguhva, bggmjutshi => kerm);
  lh : entity work.ulydeda
    port map (niwa => qjhisv);
  eydpjfmds : entity work.ulydeda
    port map (niwa => ukdkcqdqf);
end fao;



-- Seed after: 8956078669022229524,13903879141658024201
