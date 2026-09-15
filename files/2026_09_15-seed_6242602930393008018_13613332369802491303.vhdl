-- Seed: 6242602930393008018,13613332369802491303

entity q is
  port (n : inout integer_vector(3 downto 3));
end q;

architecture pbldoyb of q is
  
begin
  -- Single-driven assignments
  n <= (others => 12242);
end pbldoyb;

library ieee;
use ieee.std_logic_1164.all;

entity nnmzwyyfrx is
  port (umxncwm : linkage std_logic; tltloucgm : inout real; exdp : linkage time_vector(1 downto 2); vpf : linkage time);
end nnmzwyyfrx;

architecture yfwigvm of nnmzwyyfrx is
  
begin
  -- Single-driven assignments
  tltloucgm <= tltloucgm;
end yfwigvm;

entity mqszxrkphq is
  port (ndyzemu : linkage time; pklkexm : inout integer; qc : inout bit);
end mqszxrkphq;

library ieee;
use ieee.std_logic_1164.all;

architecture tap of mqszxrkphq is
  signal fqfmtp : integer_vector(3 downto 3);
  signal zbamj : integer_vector(3 downto 3);
  signal htrhm : time;
  signal jos : time_vector(1 downto 2);
  signal csznaslsw : real;
  signal qtebwufma : std_logic;
begin
  zrxle : entity work.nnmzwyyfrx
    port map (umxncwm => qtebwufma, tltloucgm => csznaslsw, exdp => jos, vpf => htrhm);
  mybedupitp : entity work.q
    port map (n => zbamj);
  io : entity work.q
    port map (n => fqfmtp);
  
  -- Multi-driven assignments
  qtebwufma <= '-';
  qtebwufma <= 'U';
  qtebwufma <= qtebwufma;
end tap;

entity mqt is
  port (c : inout real; igra : buffer integer_vector(4 to 1));
end mqt;

library ieee;
use ieee.std_logic_1164.all;

architecture vjikac of mqt is
  signal yxyyct : integer_vector(3 downto 3);
  signal lee : time;
  signal bukvok : time_vector(1 downto 2);
  signal sfbgfmze : std_logic;
  signal qatax : time;
  signal t : time_vector(1 downto 2);
  signal nec : real;
  signal uvznaqjcqm : std_logic;
begin
  gkdiomteyn : entity work.nnmzwyyfrx
    port map (umxncwm => uvznaqjcqm, tltloucgm => nec, exdp => t, vpf => qatax);
  nhgdcti : entity work.nnmzwyyfrx
    port map (umxncwm => sfbgfmze, tltloucgm => c, exdp => bukvok, vpf => lee);
  rvuywtwi : entity work.q
    port map (n => yxyyct);
  
  -- Single-driven assignments
  igra <= igra;
  
  -- Multi-driven assignments
  sfbgfmze <= uvznaqjcqm;
  uvznaqjcqm <= '0';
end vjikac;



-- Seed after: 10462243085627958895,13613332369802491303
