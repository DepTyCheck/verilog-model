-- Seed: 1088801213719817895,6176025545472508329

entity mfni is
  port (rvmysgnc : in time);
end mfni;

architecture qdeziwzcf of mfni is
  
begin
  
end qdeziwzcf;

entity thhhli is
  port (ojr : out time);
end thhhli;

architecture xk of thhhli is
  signal kxlrwn : time;
begin
  h : entity work.mfni
    port map (rvmysgnc => ojr);
  wys : entity work.mfni
    port map (rvmysgnc => kxlrwn);
  lbvp : entity work.mfni
    port map (rvmysgnc => ojr);
  
  -- Single-driven assignments
  kxlrwn <= 8#3_6.6253# ps;
  ojr <= ojr;
end xk;



-- Seed after: 9101386121768951189,6176025545472508329
