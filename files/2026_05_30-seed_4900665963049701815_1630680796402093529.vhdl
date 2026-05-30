-- Seed: 4900665963049701815,1630680796402093529



entity mcemz is
  port (lgiwea : in boolean);
end mcemz;



architecture ikwqoigfu of mcemz is
  
begin
  
end ikwqoigfu;



entity ggf is
  port (ohbejwto : buffer boolean);
end ggf;



architecture gatjabw of ggf is
  
begin
  udeoigffbm : entity work.mcemz
    port map (lgiwea => ohbejwto);
end gatjabw;



entity yhcwjtlhb is
  port (gwfhshhoxg : buffer integer; ojjdydvjoe : buffer bit_vector(1 to 0));
end yhcwjtlhb;



architecture jk of yhcwjtlhb is
  signal w : boolean;
  signal bqrmopn : boolean;
begin
  cjnqronocq : entity work.mcemz
    port map (lgiwea => bqrmopn);
  cqscgk : entity work.mcemz
    port map (lgiwea => bqrmopn);
  xcb : entity work.ggf
    port map (ohbejwto => bqrmopn);
  qpwt : entity work.mcemz
    port map (lgiwea => w);
end jk;



-- Seed after: 4621736429618565968,1630680796402093529
