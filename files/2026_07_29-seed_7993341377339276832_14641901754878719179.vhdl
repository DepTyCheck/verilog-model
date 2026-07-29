-- Seed: 7993341377339276832,14641901754878719179

library ieee;
use ieee.std_logic_1164.all;

entity xcpr is
  port (g : in std_logic_vector(2 to 4); yatdtmyd : buffer time; aiyneiujl : in time; tdeldjy : inout integer);
end xcpr;

architecture nbuuuo of xcpr is
  
begin
  -- Single-driven assignments
  tdeldjy <= 4;
  yatdtmyd <= 16#3_1# us;
end nbuuuo;

library ieee;
use ieee.std_logic_1164.all;

entity pgkdcvengg is
  port (tjyy : out bit; basc : linkage std_logic);
end pgkdcvengg;

architecture tet of pgkdcvengg is
  
begin
  -- Single-driven assignments
  tjyy <= tjyy;
end tet;

library ieee;
use ieee.std_logic_1164.all;

entity laddoijzzv is
  port (hrcoeci : in std_logic_vector(3 downto 4); tzhsp : inout integer; wjrzlpeh : buffer std_logic; yoqfn : linkage std_logic);
end laddoijzzv;

library ieee;
use ieee.std_logic_1164.all;

architecture zqhskcykt of laddoijzzv is
  signal krudmfzp : std_logic;
  signal azdap : bit;
  signal kxsxhpfyj : integer;
  signal emswkcnqib : time;
  signal sgso : integer;
  signal dnxr : time;
  signal araxpk : time;
  signal tr : std_logic_vector(2 to 4);
begin
  tcm : entity work.xcpr
    port map (g => tr, yatdtmyd => araxpk, aiyneiujl => dnxr, tdeldjy => sgso);
  byctmlbtu : entity work.xcpr
    port map (g => tr, yatdtmyd => emswkcnqib, aiyneiujl => emswkcnqib, tdeldjy => kxsxhpfyj);
  hc : entity work.xcpr
    port map (g => tr, yatdtmyd => dnxr, aiyneiujl => dnxr, tdeldjy => tzhsp);
  ptclgj : entity work.pgkdcvengg
    port map (tjyy => azdap, basc => krudmfzp);
  
  -- Multi-driven assignments
  krudmfzp <= wjrzlpeh;
  wjrzlpeh <= '0';
  wjrzlpeh <= wjrzlpeh;
end zqhskcykt;



-- Seed after: 11048814285049501865,14641901754878719179
