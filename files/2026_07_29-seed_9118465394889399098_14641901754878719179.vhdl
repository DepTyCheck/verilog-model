-- Seed: 9118465394889399098,14641901754878719179

library ieee;
use ieee.std_logic_1164.all;

entity rrwihc is
  port (s : linkage time; atxwt : linkage string(3 to 2); nomsbbl : buffer real_vector(4 downto 3); y : linkage std_logic);
end rrwihc;

architecture swq of rrwihc is
  
begin
  -- Single-driven assignments
  nomsbbl <= (40.41, 8#04.6555#);
end swq;

library ieee;
use ieee.std_logic_1164.all;

entity ugps is
  port (wrwushe : inout time; okmvuetif : out std_logic; t : inout std_logic_vector(3 downto 3));
end ugps;

library ieee;
use ieee.std_logic_1164.all;

architecture s of ugps is
  signal onthtez : real_vector(4 downto 3);
  signal dld : string(3 to 2);
  signal yq : time;
  signal pwahhkh : std_logic;
  signal npxplango : real_vector(4 downto 3);
  signal ar : string(3 to 2);
  signal qkzysrvnam : time;
  signal bgoof : std_logic;
  signal brs : real_vector(4 downto 3);
  signal ran : string(3 to 2);
  signal snspuyqyxw : time;
begin
  vphgjjumy : entity work.rrwihc
    port map (s => snspuyqyxw, atxwt => ran, nomsbbl => brs, y => bgoof);
  jyma : entity work.rrwihc
    port map (s => qkzysrvnam, atxwt => ar, nomsbbl => npxplango, y => pwahhkh);
  mzsdvvk : entity work.rrwihc
    port map (s => yq, atxwt => dld, nomsbbl => onthtez, y => pwahhkh);
  
  -- Multi-driven assignments
  bgoof <= okmvuetif;
  t <= t;
  t <= (others => '1');
end s;

entity capgetbo is
  port (lnhnvrr : in integer; ngwyhemzgo : linkage bit_vector(1 to 1));
end capgetbo;

library ieee;
use ieee.std_logic_1164.all;

architecture lwnqb of capgetbo is
  signal bm : real_vector(4 downto 3);
  signal m : string(3 to 2);
  signal nnjiau : time;
  signal ra : std_logic_vector(3 downto 3);
  signal a : std_logic;
  signal oejmwsgkq : time;
  signal awduav : std_logic;
  signal foavq : real_vector(4 downto 3);
  signal kdnkvapbdt : string(3 to 2);
  signal zcmvfn : time;
begin
  roajqvw : entity work.rrwihc
    port map (s => zcmvfn, atxwt => kdnkvapbdt, nomsbbl => foavq, y => awduav);
  ixvfahpkt : entity work.ugps
    port map (wrwushe => oejmwsgkq, okmvuetif => a, t => ra);
  dp : entity work.rrwihc
    port map (s => nnjiau, atxwt => m, nomsbbl => bm, y => awduav);
  
  -- Multi-driven assignments
  awduav <= awduav;
  awduav <= awduav;
  a <= awduav;
end lwnqb;

library ieee;
use ieee.std_logic_1164.all;

entity atszjwk is
  port (g : buffer time; bikuaywzzu : inout std_logic_vector(0 to 0); hym : in severity_level);
end atszjwk;

library ieee;
use ieee.std_logic_1164.all;

architecture cfjz of atszjwk is
  signal fdliju : real_vector(4 downto 3);
  signal kgmpmusnj : string(3 to 2);
  signal xfwo : time;
  signal izlls : std_logic;
  signal nmvdmo : real_vector(4 downto 3);
  signal esne : string(3 to 2);
  signal lgabcp : time;
  signal mvlrux : bit_vector(1 to 1);
  signal aea : integer;
begin
  al : entity work.capgetbo
    port map (lnhnvrr => aea, ngwyhemzgo => mvlrux);
  t : entity work.rrwihc
    port map (s => lgabcp, atxwt => esne, nomsbbl => nmvdmo, y => izlls);
  grtecvte : entity work.rrwihc
    port map (s => xfwo, atxwt => kgmpmusnj, nomsbbl => fdliju, y => izlls);
  
  -- Multi-driven assignments
  bikuaywzzu <= bikuaywzzu;
  bikuaywzzu <= "0";
  bikuaywzzu <= (others => 'Z');
end cfjz;



-- Seed after: 15947659313481941503,14641901754878719179
