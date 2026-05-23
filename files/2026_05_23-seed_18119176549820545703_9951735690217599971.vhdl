-- Seed: 18119176549820545703,9951735690217599971



entity qv is
  port (hxulxvtu : out time; npryz : buffer bit);
end qv;



architecture auysewtfcn of qv is
  
begin
  
end auysewtfcn;



entity oswrjln is
  port (ogbt : buffer integer_vector(0 downto 4));
end oswrjln;



architecture ooezrulipl of oswrjln is
  signal sewphrbpoi : bit;
  signal eztofr : time;
  signal hbe : bit;
  signal wgdgp : time;
begin
  b : entity work.qv
    port map (hxulxvtu => wgdgp, npryz => hbe);
  rywkvtyc : entity work.qv
    port map (hxulxvtu => eztofr, npryz => sewphrbpoi);
end ooezrulipl;



entity rrp is
  port (ct : buffer bit; uuslykh : buffer time);
end rrp;



architecture qotgfj of rrp is
  signal a : integer_vector(0 downto 4);
  signal lrtpqbh : integer_vector(0 downto 4);
begin
  zll : entity work.oswrjln
    port map (ogbt => lrtpqbh);
  vccb : entity work.oswrjln
    port map (ogbt => a);
end qotgfj;



-- Seed after: 1138211737026773045,9951735690217599971
