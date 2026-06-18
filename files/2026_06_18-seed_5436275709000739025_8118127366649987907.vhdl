-- Seed: 5436275709000739025,8118127366649987907

library ieee;
use ieee.std_logic_1164.all;

entity bkmdp is
  port (x : in real; eqrjjztkbd : inout std_logic_vector(0 to 0); liybeqh : in real_vector(0 downto 4));
end bkmdp;

architecture pytzqgmlcy of bkmdp is
  
begin
  -- Multi-driven assignments
  eqrjjztkbd <= "0";
  eqrjjztkbd <= "U";
end pytzqgmlcy;

entity qyvrcset is
  port (subnaa : in real; fdbzskrmg : in boolean_vector(0 to 1); xbswufllva : out integer);
end qyvrcset;

library ieee;
use ieee.std_logic_1164.all;

architecture c of qyvrcset is
  signal bjuc : std_logic_vector(0 to 0);
  signal eapzoux : real;
  signal j : real_vector(0 downto 4);
  signal todebeyf : std_logic_vector(0 to 0);
begin
  xth : entity work.bkmdp
    port map (x => subnaa, eqrjjztkbd => todebeyf, liybeqh => j);
  bh : entity work.bkmdp
    port map (x => eapzoux, eqrjjztkbd => bjuc, liybeqh => j);
  
  -- Single-driven assignments
  j <= (others => 0.0);
  xbswufllva <= 3;
  
  -- Multi-driven assignments
  todebeyf <= (others => 'X');
  todebeyf <= "0";
  todebeyf <= (others => 'X');
  bjuc <= (others => 'U');
end c;

library ieee;
use ieee.std_logic_1164.all;

entity aypj is
  port (bnftrzyw : buffer time; aqoqrtvd : inout std_logic);
end aypj;

library ieee;
use ieee.std_logic_1164.all;

architecture fz of aypj is
  signal qi : real_vector(0 downto 4);
  signal oro : std_logic_vector(0 to 0);
  signal uwiqe : real;
begin
  vr : entity work.bkmdp
    port map (x => uwiqe, eqrjjztkbd => oro, liybeqh => qi);
  
  -- Multi-driven assignments
  aqoqrtvd <= '-';
  aqoqrtvd <= '1';
  aqoqrtvd <= 'W';
end fz;

library ieee;
use ieee.std_logic_1164.all;

entity tf is
  port (srkeysb : buffer std_logic_vector(1 downto 3); sdqtmojtrz : linkage bit);
end tf;

library ieee;
use ieee.std_logic_1164.all;

architecture hedrkghes of tf is
  signal tqgqmcczre : real_vector(0 downto 4);
  signal cxrvqgjazf : std_logic_vector(0 to 0);
  signal rsvzgcjn : real;
begin
  yqhfcsdtuh : entity work.bkmdp
    port map (x => rsvzgcjn, eqrjjztkbd => cxrvqgjazf, liybeqh => tqgqmcczre);
end hedrkghes;



-- Seed after: 1589342087158742652,8118127366649987907
