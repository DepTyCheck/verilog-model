-- Seed: 3882048339050601238,16265041255589496407



entity vrgotfsn is
  port (aftf : buffer bit_vector(0 downto 2); qo : inout real);
end vrgotfsn;



architecture dd of vrgotfsn is
  
begin
  
end dd;



entity n is
  port (vyjglp : in real; msxz : out integer; rxnak : in integer; hxkbr : inout integer_vector(0 downto 2));
end n;



architecture qquzioasj of n is
  signal ytownt : real;
  signal qiprdbzcoa : bit_vector(0 downto 2);
begin
  gwfkspsnp : entity work.vrgotfsn
    port map (aftf => qiprdbzcoa, qo => ytownt);
end qquzioasj;

library ieee;
use ieee.std_logic_1164.all;

entity qau is
  port (rheoch : in integer; orv : out std_logic; vv : in real);
end qau;



architecture j of qau is
  signal gvw : real;
  signal nfcdhya : bit_vector(0 downto 2);
  signal nluhgl : real;
  signal nihkqeqa : bit_vector(0 downto 2);
  signal ncviwfdxwp : real;
  signal ktbrzbwmtm : bit_vector(0 downto 2);
begin
  eovmdn : entity work.vrgotfsn
    port map (aftf => ktbrzbwmtm, qo => ncviwfdxwp);
  tng : entity work.vrgotfsn
    port map (aftf => nihkqeqa, qo => nluhgl);
  aglsoikhg : entity work.vrgotfsn
    port map (aftf => nfcdhya, qo => gvw);
end j;



entity mucvx is
  port (fil : out boolean);
end mucvx;



architecture doqulvrh of mucvx is
  signal fwzpdibjav : integer_vector(0 downto 2);
  signal pqizcvugx : integer;
  signal spqrftatob : real;
  signal ohgqyh : real;
  signal tlp : bit_vector(0 downto 2);
  signal nrtptralro : integer_vector(0 downto 2);
  signal ztmdq : integer;
  signal uzmbwcg : integer;
  signal rfwfii : real;
  signal bzryzfue : bit_vector(0 downto 2);
begin
  bsxboht : entity work.vrgotfsn
    port map (aftf => bzryzfue, qo => rfwfii);
  egdlxzytg : entity work.n
    port map (vyjglp => rfwfii, msxz => uzmbwcg, rxnak => ztmdq, hxkbr => nrtptralro);
  a : entity work.vrgotfsn
    port map (aftf => tlp, qo => ohgqyh);
  nrlgkmdz : entity work.n
    port map (vyjglp => spqrftatob, msxz => pqizcvugx, rxnak => uzmbwcg, hxkbr => fwzpdibjav);
end doqulvrh;



-- Seed after: 5258625469724801731,16265041255589496407
