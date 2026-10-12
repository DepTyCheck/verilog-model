-- Seed: 14037191942161858732,3426943171337919827

library ieee;
use ieee.std_logic_1164.all;

entity wyjpxcjs is
  port (viumk : linkage real_vector(4 to 1); etp : in std_logic_vector(3 to 3); rbxcrjqvz : inout integer);
end wyjpxcjs;

architecture anvzwwqqc of wyjpxcjs is
  
begin
  -- Single-driven assignments
  rbxcrjqvz <= 16#F_1#;
end anvzwwqqc;

entity fptwdtqiam is
  port (tlppe : inout integer);
end fptwdtqiam;

library ieee;
use ieee.std_logic_1164.all;

architecture ttvjpry of fptwdtqiam is
  signal zsy : integer;
  signal iv : std_logic_vector(3 to 3);
  signal jchknnjdp : real_vector(4 to 1);
begin
  izgvubim : entity work.wyjpxcjs
    port map (viumk => jchknnjdp, etp => iv, rbxcrjqvz => zsy);
  
  -- Single-driven assignments
  tlppe <= tlppe;
end ttvjpry;

entity exorx is
  port (fnxjgwjd : buffer time);
end exorx;

library ieee;
use ieee.std_logic_1164.all;

architecture txnzxemssm of exorx is
  signal a : integer;
  signal uw : std_logic_vector(3 to 3);
  signal axdgtwjprn : real_vector(4 to 1);
  signal i : integer;
  signal ytwcvj : integer;
  signal zslw : std_logic_vector(3 to 3);
  signal zjanlbythg : real_vector(4 to 1);
begin
  p : entity work.wyjpxcjs
    port map (viumk => zjanlbythg, etp => zslw, rbxcrjqvz => ytwcvj);
  az : entity work.fptwdtqiam
    port map (tlppe => i);
  fpcue : entity work.wyjpxcjs
    port map (viumk => axdgtwjprn, etp => uw, rbxcrjqvz => a);
  
  -- Multi-driven assignments
  zslw <= zslw;
  uw <= (others => '1');
end txnzxemssm;

library ieee;
use ieee.std_logic_1164.all;

entity r is
  port (wiqyakg : inout boolean; jlrg : buffer std_logic_vector(1 to 1); enlcy : buffer boolean_vector(4 downto 1); uxn : buffer time);
end r;

architecture aeltlelbzc of r is
  
begin
  -- Single-driven assignments
  uxn <= 43.0141 us;
  wiqyakg <= TRUE;
  enlcy <= (FALSE, TRUE, FALSE, FALSE);
  
  -- Multi-driven assignments
  jlrg <= (others => 'U');
end aeltlelbzc;



-- Seed after: 15997190333662001952,3426943171337919827
