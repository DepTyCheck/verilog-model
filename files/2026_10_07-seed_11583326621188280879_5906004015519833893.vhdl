-- Seed: 11583326621188280879,5906004015519833893

library ieee;
use ieee.std_logic_1164.all;

entity ypud is
  port (czzjmjzn : inout std_logic; cfjyehiu : inout std_logic_vector(3 to 0); me : in string(3 downto 3); fad : inout real);
end ypud;

architecture ngjrngkfem of ypud is
  
begin
  -- Single-driven assignments
  fad <= 323.34103;
end ngjrngkfem;

library ieee;
use ieee.std_logic_1164.all;

entity fcnhetwgzc is
  port (itcbczp : buffer integer; uigeozmi : in std_logic_vector(3 downto 2));
end fcnhetwgzc;

library ieee;
use ieee.std_logic_1164.all;

architecture rfgzhto of fcnhetwgzc is
  signal fp : real;
  signal zabp : std_logic_vector(3 to 0);
  signal fgohl : std_logic;
  signal itl : real;
  signal ukk : std_logic_vector(3 to 0);
  signal nhrodt : std_logic;
  signal rssibva : real;
  signal qw : string(3 downto 3);
  signal ryjrkze : std_logic_vector(3 to 0);
  signal kz : std_logic;
begin
  xdg : entity work.ypud
    port map (czzjmjzn => kz, cfjyehiu => ryjrkze, me => qw, fad => rssibva);
  rl : entity work.ypud
    port map (czzjmjzn => nhrodt, cfjyehiu => ukk, me => qw, fad => itl);
  ne : entity work.ypud
    port map (czzjmjzn => fgohl, cfjyehiu => zabp, me => qw, fad => fp);
  
  -- Single-driven assignments
  qw <= "k";
  itcbczp <= itcbczp;
end rfgzhto;

entity fnxqu is
  port (wjlbhlx : out integer; yqn : in real);
end fnxqu;

library ieee;
use ieee.std_logic_1164.all;

architecture gq of fnxqu is
  signal ymuvrr : real;
  signal lwndlv : std_logic_vector(3 to 0);
  signal hpextccim : real;
  signal hdyafflqsu : string(3 downto 3);
  signal cbuuq : std_logic_vector(3 to 0);
  signal fjxvddrjd : real;
  signal o : string(3 downto 3);
  signal aoyvxzr : std_logic_vector(3 to 0);
  signal zhezpbjxnh : std_logic;
begin
  u : entity work.ypud
    port map (czzjmjzn => zhezpbjxnh, cfjyehiu => aoyvxzr, me => o, fad => fjxvddrjd);
  nprfgt : entity work.ypud
    port map (czzjmjzn => zhezpbjxnh, cfjyehiu => cbuuq, me => hdyafflqsu, fad => hpextccim);
  vqas : entity work.ypud
    port map (czzjmjzn => zhezpbjxnh, cfjyehiu => lwndlv, me => o, fad => ymuvrr);
  
  -- Single-driven assignments
  wjlbhlx <= 8#0_1#;
  
  -- Multi-driven assignments
  zhezpbjxnh <= '0';
  aoyvxzr <= (others => '0');
  zhezpbjxnh <= 'X';
end gq;



-- Seed after: 7758516419513643567,5906004015519833893
