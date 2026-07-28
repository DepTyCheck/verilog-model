-- Seed: 13320096533522251967,2511821214772927453

library ieee;
use ieee.std_logic_1164.all;

entity xifqanh is
  port (rrnt : linkage bit_vector(3 downto 0); ljqa : out boolean; gyppomozd : inout std_logic_vector(4 downto 4));
end xifqanh;

architecture ykhwsbpie of xifqanh is
  
begin
  -- Single-driven assignments
  ljqa <= TRUE;
  
  -- Multi-driven assignments
  gyppomozd <= gyppomozd;
end ykhwsbpie;

library ieee;
use ieee.std_logic_1164.all;

entity bmsqkodu is
  port (lkc : in severity_level; l : inout time; zljeevfej : out std_logic; i : linkage integer);
end bmsqkodu;

library ieee;
use ieee.std_logic_1164.all;

architecture xognqad of bmsqkodu is
  signal vbcms : boolean;
  signal hpy : bit_vector(3 downto 0);
  signal ax : boolean;
  signal ngudcn : bit_vector(3 downto 0);
  signal gc : boolean;
  signal smm : bit_vector(3 downto 0);
  signal w : std_logic_vector(4 downto 4);
  signal wmqxy : boolean;
  signal ykgly : bit_vector(3 downto 0);
begin
  fjskqhju : entity work.xifqanh
    port map (rrnt => ykgly, ljqa => wmqxy, gyppomozd => w);
  fxteqlvhgd : entity work.xifqanh
    port map (rrnt => smm, ljqa => gc, gyppomozd => w);
  ylrpkklx : entity work.xifqanh
    port map (rrnt => ngudcn, ljqa => ax, gyppomozd => w);
  lzyhxeug : entity work.xifqanh
    port map (rrnt => hpy, ljqa => vbcms, gyppomozd => w);
  
  -- Multi-driven assignments
  w <= "1";
  w <= (others => 'H');
  zljeevfej <= zljeevfej;
end xognqad;



-- Seed after: 2738158053760334994,2511821214772927453
