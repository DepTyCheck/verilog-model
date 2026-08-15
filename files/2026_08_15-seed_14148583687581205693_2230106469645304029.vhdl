-- Seed: 14148583687581205693,2230106469645304029

library ieee;
use ieee.std_logic_1164.all;

entity ae is
  port (yt : inout time_vector(0 to 0); qyplggu : inout integer_vector(2 downto 4); nqzek : out std_logic_vector(3 downto 1));
end ae;

architecture catud of ae is
  
begin
  -- Single-driven assignments
  qyplggu <= (others => 0);
  
  -- Multi-driven assignments
  nqzek <= nqzek;
end catud;

library ieee;
use ieee.std_logic_1164.all;

entity rh is
  port (errt : out std_logic; eurg : in time; abkke : buffer std_logic_vector(0 downto 1); iyhqacqmfo : buffer std_logic);
end rh;

library ieee;
use ieee.std_logic_1164.all;

architecture xdwsuby of rh is
  signal qy : integer_vector(2 downto 4);
  signal nrzvg : time_vector(0 to 0);
  signal wt : integer_vector(2 downto 4);
  signal xngtdm : time_vector(0 to 0);
  signal c : std_logic_vector(3 downto 1);
  signal vxrtzgr : integer_vector(2 downto 4);
  signal vrqvug : time_vector(0 to 0);
begin
  jtvj : entity work.ae
    port map (yt => vrqvug, qyplggu => vxrtzgr, nqzek => c);
  tiksxxtjg : entity work.ae
    port map (yt => xngtdm, qyplggu => wt, nqzek => c);
  amporuxuo : entity work.ae
    port map (yt => nrzvg, qyplggu => qy, nqzek => c);
  
  -- Multi-driven assignments
  iyhqacqmfo <= iyhqacqmfo;
  iyhqacqmfo <= iyhqacqmfo;
end xdwsuby;



-- Seed after: 8999068512498842783,2230106469645304029
