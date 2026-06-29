-- Seed: 13859408708616106956,17047277710231705797

library ieee;
use ieee.std_logic_1164.all;

entity shywjx is
  port (ujwbenad : out string(4 to 2); btfp : in std_logic_vector(2 downto 0));
end shywjx;

architecture ehli of shywjx is
  
begin
  -- Single-driven assignments
  ujwbenad <= (others => ' ');
end ehli;

entity vgoy is
  port (ttqdyufvr : out bit_vector(2 downto 4); eweyzya : out time);
end vgoy;

library ieee;
use ieee.std_logic_1164.all;

architecture tjdyje of vgoy is
  signal cv : std_logic_vector(2 downto 0);
  signal ithik : string(4 to 2);
begin
  wbgugl : entity work.shywjx
    port map (ujwbenad => ithik, btfp => cv);
end tjdyje;

entity vl is
  port (jg : inout bit; xpfz : inout time; pvip : out bit);
end vl;

architecture hpeqg of vl is
  
begin
  -- Single-driven assignments
  jg <= '1';
end hpeqg;

library ieee;
use ieee.std_logic_1164.all;

entity im is
  port (fhoyj : linkage std_logic_vector(4 to 3));
end im;

library ieee;
use ieee.std_logic_1164.all;

architecture swewpvufn of im is
  signal c : std_logic_vector(2 downto 0);
  signal bkx : string(4 to 2);
  signal ssuusaxu : bit;
  signal eykzb : time;
  signal mssz : bit;
  signal evzxdmbmcw : bit;
  signal ykg : time;
  signal noinb : bit;
  signal q : bit;
  signal htegrpgt : time;
  signal vvdmcf : bit;
begin
  t : entity work.vl
    port map (jg => vvdmcf, xpfz => htegrpgt, pvip => q);
  nxxez : entity work.vl
    port map (jg => noinb, xpfz => ykg, pvip => evzxdmbmcw);
  msde : entity work.vl
    port map (jg => mssz, xpfz => eykzb, pvip => ssuusaxu);
  qev : entity work.shywjx
    port map (ujwbenad => bkx, btfp => c);
  
  -- Multi-driven assignments
  c <= ('0', 'H', '-');
  c <= ('W', 'U', '1');
  c <= ('U', 'Z', '-');
end swewpvufn;



-- Seed after: 5032277960137657113,17047277710231705797
