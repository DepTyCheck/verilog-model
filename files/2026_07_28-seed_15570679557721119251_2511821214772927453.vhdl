-- Seed: 15570679557721119251,2511821214772927453

library ieee;
use ieee.std_logic_1164.all;

entity hd is
  port (ft : linkage std_logic; qmrveug : in integer_vector(4 downto 0); nuwkbkd : out real);
end hd;

architecture xqqkz of hd is
  
begin
  -- Single-driven assignments
  nuwkbkd <= nuwkbkd;
end xqqkz;

library ieee;
use ieee.std_logic_1164.all;

entity kqeoal is
  port (serefmgadh : buffer std_logic; cvru : linkage real; ujnqmmwbh : inout std_logic);
end kqeoal;

library ieee;
use ieee.std_logic_1164.all;

architecture egihg of kqeoal is
  signal zrerawo : real;
  signal znuwgdg : real;
  signal ksutcb : integer_vector(4 downto 0);
  signal b : real;
  signal jh : integer_vector(4 downto 0);
  signal qb : std_logic;
  signal ddba : real;
  signal bqdlaynjq : integer_vector(4 downto 0);
  signal anjo : std_logic;
begin
  ulekg : entity work.hd
    port map (ft => anjo, qmrveug => bqdlaynjq, nuwkbkd => ddba);
  jtpo : entity work.hd
    port map (ft => qb, qmrveug => jh, nuwkbkd => b);
  irhdbqje : entity work.hd
    port map (ft => serefmgadh, qmrveug => ksutcb, nuwkbkd => znuwgdg);
  vpaoez : entity work.hd
    port map (ft => ujnqmmwbh, qmrveug => ksutcb, nuwkbkd => zrerawo);
  
  -- Single-driven assignments
  bqdlaynjq <= (16#9E8#, 22, 3_2_3, 2#10#, 13);
  
  -- Multi-driven assignments
  ujnqmmwbh <= '-';
  anjo <= qb;
  ujnqmmwbh <= '0';
end egihg;



-- Seed after: 11099050679109169018,2511821214772927453
