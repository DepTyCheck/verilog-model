-- Seed: 845221113142231564,13857275728440271305

entity hhgedgia is
  port (sieqttnu : buffer real; wvlqs : linkage bit_vector(4 downto 2); joorbjrfa : linkage time; xvwhefjrmm : inout time);
end hhgedgia;

architecture pcgyfpn of hhgedgia is
  
begin
  -- Single-driven assignments
  xvwhefjrmm <= xvwhefjrmm;
  sieqttnu <= sieqttnu;
end pcgyfpn;

entity n is
  port (bzyeofl : in real_vector(2 downto 3); bckpej : linkage bit; tjoiczq : in bit_vector(0 to 2));
end n;

architecture yg of n is
  signal bfsucyypen : time;
  signal ic : time;
  signal w : bit_vector(4 downto 2);
  signal xjxxlb : real;
  signal qjgv : time;
  signal fnwt : time;
  signal trtjnp : bit_vector(4 downto 2);
  signal rymjmbnwq : real;
  signal nuqxtzao : time;
  signal awadyyhkzg : time;
  signal mewzuko : bit_vector(4 downto 2);
  signal ivsomivi : real;
begin
  y : entity work.hhgedgia
    port map (sieqttnu => ivsomivi, wvlqs => mewzuko, joorbjrfa => awadyyhkzg, xvwhefjrmm => nuqxtzao);
  c : entity work.hhgedgia
    port map (sieqttnu => rymjmbnwq, wvlqs => trtjnp, joorbjrfa => fnwt, xvwhefjrmm => qjgv);
  kkbqiep : entity work.hhgedgia
    port map (sieqttnu => xjxxlb, wvlqs => w, joorbjrfa => ic, xvwhefjrmm => bfsucyypen);
end yg;

library ieee;
use ieee.std_logic_1164.all;

entity yoyzr is
  port (tbchujf : out std_logic_vector(2 downto 3); euvealtqp : linkage std_logic_vector(1 downto 0));
end yoyzr;

architecture xjkokjt of yoyzr is
  signal xlevrec : bit_vector(0 to 2);
  signal nkbxs : bit;
  signal eoegxjcwui : real_vector(2 downto 3);
begin
  wkgmpuzw : entity work.n
    port map (bzyeofl => eoegxjcwui, bckpej => nkbxs, tjoiczq => xlevrec);
  
  -- Single-driven assignments
  eoegxjcwui <= eoegxjcwui;
  xlevrec <= xlevrec;
  
  -- Multi-driven assignments
  tbchujf <= tbchujf;
  tbchujf <= "";
  tbchujf <= (others => '0');
end xjkokjt;



-- Seed after: 10187965859152460361,13857275728440271305
