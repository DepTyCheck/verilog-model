-- Seed: 4390817722233685364,10594830431004325987

entity lkpuraslic is
  port (dhpizj : out time);
end lkpuraslic;

architecture e of lkpuraslic is
  
begin
  -- Single-driven assignments
  dhpizj <= 4 min;
end e;

entity bzw is
  port (cjcqfam : inout bit_vector(1 to 0); sjasmsrrzz : out integer; p : in boolean);
end bzw;

architecture bbv of bzw is
  signal ovif : time;
  signal zdvvgwmt : time;
  signal pzvazvmton : time;
  signal tlikeeoqah : time;
begin
  nmcqwhe : entity work.lkpuraslic
    port map (dhpizj => tlikeeoqah);
  fb : entity work.lkpuraslic
    port map (dhpizj => pzvazvmton);
  hiyohpzuy : entity work.lkpuraslic
    port map (dhpizj => zdvvgwmt);
  lxjd : entity work.lkpuraslic
    port map (dhpizj => ovif);
end bbv;

library ieee;
use ieee.std_logic_1164.all;

entity spvx is
  port (fmskjpus : buffer std_logic_vector(3 downto 3); lp : in std_logic);
end spvx;

architecture tx of spvx is
  signal rkvqkqeed : boolean;
  signal ilqcvnsx : integer;
  signal oc : bit_vector(1 to 0);
begin
  mpub : entity work.bzw
    port map (cjcqfam => oc, sjasmsrrzz => ilqcvnsx, p => rkvqkqeed);
  
  -- Single-driven assignments
  rkvqkqeed <= TRUE;
  
  -- Multi-driven assignments
  fmskjpus <= fmskjpus;
  fmskjpus <= "Z";
  fmskjpus <= fmskjpus;
end tx;



-- Seed after: 512551901431110155,10594830431004325987
