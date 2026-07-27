-- Seed: 3562121321855981346,662889661651915549

library ieee;
use ieee.std_logic_1164.all;

entity qbemkpbxn is
  port (mfqjxnkvmp : in std_logic_vector(4 downto 2); ittws : in std_logic);
end qbemkpbxn;

architecture zshj of qbemkpbxn is
  
begin
  
end zshj;

library ieee;
use ieee.std_logic_1164.all;

entity txnjrnzx is
  port (lmoqedam : in integer; tmblzxknt : inout boolean_vector(4 downto 4); odyccggesv : inout std_logic);
end txnjrnzx;

library ieee;
use ieee.std_logic_1164.all;

architecture lufuyncyg of txnjrnzx is
  signal zgzfpxkpag : std_logic;
  signal vjah : std_logic_vector(4 downto 2);
  signal ylbbhp : std_logic;
  signal pjc : std_logic_vector(4 downto 2);
begin
  ohgyoiu : entity work.qbemkpbxn
    port map (mfqjxnkvmp => pjc, ittws => odyccggesv);
  kqrgjazh : entity work.qbemkpbxn
    port map (mfqjxnkvmp => pjc, ittws => ylbbhp);
  iuy : entity work.qbemkpbxn
    port map (mfqjxnkvmp => vjah, ittws => zgzfpxkpag);
  
  -- Single-driven assignments
  tmblzxknt <= (others => FALSE);
end lufuyncyg;



-- Seed after: 14824767094427636159,662889661651915549
