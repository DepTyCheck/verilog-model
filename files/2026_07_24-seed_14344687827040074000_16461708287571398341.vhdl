-- Seed: 14344687827040074000,16461708287571398341

library ieee;
use ieee.std_logic_1164.all;

entity mp is
  port (uxj : out time_vector(0 downto 1); t : linkage integer; hvj : inout std_logic_vector(4 downto 1); xxlij : out time);
end mp;

architecture msrnoj of mp is
  
begin
  -- Single-driven assignments
  xxlij <= 3.3_3_3_1 us;
end msrnoj;

library ieee;
use ieee.std_logic_1164.all;

entity ymfa is
  port (dknxavfrl : out time_vector(3 downto 3); gh : in time_vector(1 downto 3); sny : out std_logic; xivrm : out integer);
end ymfa;

library ieee;
use ieee.std_logic_1164.all;

architecture qn of ymfa is
  signal ljijc : time;
  signal mttgzsrxu : time_vector(0 downto 1);
  signal gwxne : time;
  signal jzybyhfskz : std_logic_vector(4 downto 1);
  signal my : integer;
  signal pexnymdp : time_vector(0 downto 1);
begin
  fws : entity work.mp
    port map (uxj => pexnymdp, t => my, hvj => jzybyhfskz, xxlij => gwxne);
  ui : entity work.mp
    port map (uxj => mttgzsrxu, t => xivrm, hvj => jzybyhfskz, xxlij => ljijc);
  
  -- Multi-driven assignments
  sny <= 'U';
  sny <= '0';
  sny <= sny;
end qn;

entity so is
  port (e : in real; vek : inout integer_vector(1 downto 1));
end so;

library ieee;
use ieee.std_logic_1164.all;

architecture dw of so is
  signal wjdi : time;
  signal zxjyv : integer;
  signal syiqoc : time_vector(0 downto 1);
  signal yrvs : time;
  signal v : integer;
  signal qkfmziu : time_vector(0 downto 1);
  signal hfwrzvg : time;
  signal sw : std_logic_vector(4 downto 1);
  signal r : integer;
  signal yn : time_vector(0 downto 1);
begin
  wme : entity work.mp
    port map (uxj => yn, t => r, hvj => sw, xxlij => hfwrzvg);
  vb : entity work.mp
    port map (uxj => qkfmziu, t => v, hvj => sw, xxlij => yrvs);
  anrk : entity work.mp
    port map (uxj => syiqoc, t => zxjyv, hvj => sw, xxlij => wjdi);
  
  -- Single-driven assignments
  vek <= (others => 8#2_3_4#);
  
  -- Multi-driven assignments
  sw <= ('-', '0', '-', '0');
  sw <= ('U', '-', 'W', 'X');
  sw <= sw;
  sw <= "LZZZ";
end dw;



-- Seed after: 5094478719869591037,16461708287571398341
