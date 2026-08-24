-- Seed: 4759926196634614832,16159265764638711791

entity c is
  port (z : linkage boolean_vector(0 downto 3); b : inout character; jbe : buffer integer);
end c;

architecture oxfd of c is
  
begin
  -- Single-driven assignments
  b <= 'u';
  jbe <= 16#C860#;
end oxfd;

library ieee;
use ieee.std_logic_1164.all;

entity faqc is
  port (r : linkage integer; qfloxdmp : inout std_logic_vector(4 to 1); vlkrjbc : in std_logic; qo : inout real);
end faqc;

architecture p of faqc is
  signal wswlqcyfm : integer;
  signal dbuonr : character;
  signal fue : boolean_vector(0 downto 3);
begin
  raoh : entity work.c
    port map (z => fue, b => dbuonr, jbe => wswlqcyfm);
  
  -- Single-driven assignments
  qo <= qo;
  
  -- Multi-driven assignments
  qfloxdmp <= (others => '0');
  qfloxdmp <= qfloxdmp;
end p;

entity qubtvag is
  port (tjfsil : in real; yx : out bit_vector(2 to 0));
end qubtvag;

architecture ircxetmak of qubtvag is
  signal xd : integer;
  signal uzr : character;
  signal vqptwxx : boolean_vector(0 downto 3);
begin
  k : entity work.c
    port map (z => vqptwxx, b => uzr, jbe => xd);
end ircxetmak;

library ieee;
use ieee.std_logic_1164.all;

entity iybozchpk is
  port (mfhta : linkage std_logic_vector(4 downto 1));
end iybozchpk;

architecture wpvl of iybozchpk is
  signal wj : bit_vector(2 to 0);
  signal nhb : real;
begin
  wu : entity work.qubtvag
    port map (tjfsil => nhb, yx => wj);
  
  -- Single-driven assignments
  nhb <= 031.0_3;
end wpvl;



-- Seed after: 17194741150998452575,16159265764638711791
