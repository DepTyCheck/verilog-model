-- Seed: 1834908446472520535,5805648483995786113

entity ds is
  port (hw : buffer real; g : out boolean; t : buffer time; fijhe : out real_vector(4 to 0));
end ds;

architecture na of ds is
  
begin
  -- Single-driven assignments
  hw <= 3_0.041;
  fijhe <= (others => 0.0);
  g <= g;
end na;

library ieee;
use ieee.std_logic_1164.all;

entity uhnz is
  port (cjbexceeoa : linkage std_logic_vector(1 to 2); bguzk : in std_logic_vector(1 downto 1));
end uhnz;

architecture lmg of uhnz is
  signal lmgjffwe : real_vector(4 to 0);
  signal ply : time;
  signal eltc : boolean;
  signal xad : real;
  signal gypxte : real_vector(4 to 0);
  signal qypf : time;
  signal oblcj : boolean;
  signal zziyed : real;
  signal qjcgyiffs : real_vector(4 to 0);
  signal vepnyowvbx : time;
  signal bfpzv : boolean;
  signal v : real;
begin
  sk : entity work.ds
    port map (hw => v, g => bfpzv, t => vepnyowvbx, fijhe => qjcgyiffs);
  ytqfexv : entity work.ds
    port map (hw => zziyed, g => oblcj, t => qypf, fijhe => gypxte);
  xvcoutmtm : entity work.ds
    port map (hw => xad, g => eltc, t => ply, fijhe => lmgjffwe);
end lmg;

entity sd is
  port (zwydfuhp : out character; pbjhk : linkage bit; wqmf : out real);
end sd;

architecture wtz of sd is
  
begin
  -- Single-driven assignments
  zwydfuhp <= 'i';
  wqmf <= 2_1_3.1_0_2_1;
end wtz;



-- Seed after: 2765483679785071662,5805648483995786113
