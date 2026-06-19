-- Seed: 17529913286026420894,3108530264173481209

library ieee;
use ieee.std_logic_1164.all;

entity hmidlmzlj is
  port (cwk : out integer; h : in std_logic);
end hmidlmzlj;

architecture xahe of hmidlmzlj is
  
begin
  -- Single-driven assignments
  cwk <= 2_0_3_0_0;
end xahe;

library ieee;
use ieee.std_logic_1164.all;

entity o is
  port (pjqf : buffer std_logic);
end o;

library ieee;
use ieee.std_logic_1164.all;

architecture widvqr of o is
  signal jttwfiwan : std_logic;
  signal umzwrpfgtf : integer;
  signal pddrbfjaa : std_logic;
  signal pywhfqxsx : integer;
  signal evhoqvwh : std_logic;
  signal lljtsgk : integer;
  signal lvwcp : std_logic;
  signal hinhap : integer;
begin
  zm : entity work.hmidlmzlj
    port map (cwk => hinhap, h => lvwcp);
  dp : entity work.hmidlmzlj
    port map (cwk => lljtsgk, h => evhoqvwh);
  fyqoipp : entity work.hmidlmzlj
    port map (cwk => pywhfqxsx, h => pddrbfjaa);
  plkoxinicr : entity work.hmidlmzlj
    port map (cwk => umzwrpfgtf, h => jttwfiwan);
  
  -- Multi-driven assignments
  jttwfiwan <= 'L';
end widvqr;

entity yckmt is
  port (vpkz : out time_vector(0 to 1));
end yckmt;

library ieee;
use ieee.std_logic_1164.all;

architecture jof of yckmt is
  signal vxmg : std_logic;
  signal fuxmo : std_logic;
  signal urwcxmimk : integer;
  signal zbyzqrzt : std_logic;
  signal avubcqb : integer;
  signal hnk : std_logic;
  signal zzipv : integer;
begin
  sxhkawlk : entity work.hmidlmzlj
    port map (cwk => zzipv, h => hnk);
  e : entity work.hmidlmzlj
    port map (cwk => avubcqb, h => zbyzqrzt);
  aa : entity work.hmidlmzlj
    port map (cwk => urwcxmimk, h => fuxmo);
  pasd : entity work.o
    port map (pjqf => vxmg);
  
  -- Multi-driven assignments
  hnk <= 'Z';
  hnk <= 'L';
end jof;



-- Seed after: 2315130092533222770,3108530264173481209
