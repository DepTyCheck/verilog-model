-- Seed: 5854424304483982357,5511103086789671269

entity rs is
  port (ikqhqpgngw : in integer; wjqijatbi : linkage character);
end rs;

architecture fwjejf of rs is
  
begin
  
end fwjejf;

library ieee;
use ieee.std_logic_1164.all;

entity ghjzpwgfw is
  port (scw : linkage boolean; ywujjajpmw : linkage std_logic_vector(2 to 0); qwnjsg : out bit);
end ghjzpwgfw;

architecture khotbgtr of ghjzpwgfw is
  signal wmhm : character;
  signal dfes : integer;
  signal dowuvokfih : character;
  signal xxfni : character;
  signal vbqt : character;
  signal u : integer;
begin
  rlzh : entity work.rs
    port map (ikqhqpgngw => u, wjqijatbi => vbqt);
  tsr : entity work.rs
    port map (ikqhqpgngw => u, wjqijatbi => xxfni);
  gcdes : entity work.rs
    port map (ikqhqpgngw => u, wjqijatbi => dowuvokfih);
  wnvryalfn : entity work.rs
    port map (ikqhqpgngw => dfes, wjqijatbi => wmhm);
end khotbgtr;

entity gddfrzm is
  port (xyju : in time; pccsrxz : in real; dhqjvoj : buffer time_vector(3 downto 4));
end gddfrzm;

library ieee;
use ieee.std_logic_1164.all;

architecture bq of gddfrzm is
  signal k : bit;
  signal xt : std_logic_vector(2 to 0);
  signal mswao : boolean;
begin
  lhxbrojauo : entity work.ghjzpwgfw
    port map (scw => mswao, ywujjajpmw => xt, qwnjsg => k);
  
  -- Single-driven assignments
  dhqjvoj <= dhqjvoj;
  
  -- Multi-driven assignments
  xt <= (others => '0');
  xt <= "";
  xt <= xt;
  xt <= (others => '0');
end bq;

entity gnrn is
  port (tzot : linkage time; fkohikjobj : out boolean; stusxy : inout time);
end gnrn;

library ieee;
use ieee.std_logic_1164.all;

architecture cee of gnrn is
  signal uxxhpjkts : bit;
  signal fwhdzt : std_logic_vector(2 to 0);
  signal buzlxplj : boolean;
  signal pcylxoasmh : time_vector(3 downto 4);
  signal ehqdiwzwaw : real;
  signal bpkpthh : time;
  signal ahdhvf : character;
  signal krw : integer;
begin
  rhnqduhe : entity work.rs
    port map (ikqhqpgngw => krw, wjqijatbi => ahdhvf);
  pporzy : entity work.gddfrzm
    port map (xyju => bpkpthh, pccsrxz => ehqdiwzwaw, dhqjvoj => pcylxoasmh);
  jkbqvuxrd : entity work.ghjzpwgfw
    port map (scw => buzlxplj, ywujjajpmw => fwhdzt, qwnjsg => uxxhpjkts);
  
  -- Single-driven assignments
  stusxy <= 2#1000# ms;
  fkohikjobj <= FALSE;
  
  -- Multi-driven assignments
  fwhdzt <= fwhdzt;
end cee;



-- Seed after: 11457055810256030526,5511103086789671269
