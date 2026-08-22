-- Seed: 17529199140498384639,5805648483995786113

library ieee;
use ieee.std_logic_1164.all;

entity kuinxx is
  port (yzdgrqy : linkage std_logic; hntprmdpc : buffer real; iinaxvpeda : in std_logic_vector(2 to 1));
end kuinxx;

architecture m of kuinxx is
  
begin
  -- Single-driven assignments
  hntprmdpc <= 16#0_3_0.0#;
end m;

entity erb is
  port (warob : buffer time; uakvb : buffer integer);
end erb;

library ieee;
use ieee.std_logic_1164.all;

architecture zapgivtp of erb is
  signal wxsgvqg : real;
  signal am : std_logic_vector(2 to 1);
  signal xtege : real;
  signal jlzwlec : std_logic;
  signal aobpvk : std_logic_vector(2 to 1);
  signal mwtrkbbjka : real;
  signal ojhovxnsl : std_logic;
  signal wuktqq : std_logic_vector(2 to 1);
  signal crq : real;
  signal cxqgl : std_logic;
begin
  umeksrxjs : entity work.kuinxx
    port map (yzdgrqy => cxqgl, hntprmdpc => crq, iinaxvpeda => wuktqq);
  lvy : entity work.kuinxx
    port map (yzdgrqy => ojhovxnsl, hntprmdpc => mwtrkbbjka, iinaxvpeda => aobpvk);
  mqvd : entity work.kuinxx
    port map (yzdgrqy => jlzwlec, hntprmdpc => xtege, iinaxvpeda => am);
  guebzybs : entity work.kuinxx
    port map (yzdgrqy => cxqgl, hntprmdpc => wxsgvqg, iinaxvpeda => am);
  
  -- Single-driven assignments
  uakvb <= 2;
  warob <= warob;
end zapgivtp;

entity qjqjgk is
  port (cqeu : buffer time; upagipkz : inout real);
end qjqjgk;

library ieee;
use ieee.std_logic_1164.all;

architecture xfkuq of qjqjgk is
  signal bbododh : real;
  signal dauqwd : std_logic;
  signal adgzl : std_logic_vector(2 to 1);
  signal gvzoggsejw : real;
  signal qxvh : std_logic;
begin
  mkwfniqzgq : entity work.kuinxx
    port map (yzdgrqy => qxvh, hntprmdpc => gvzoggsejw, iinaxvpeda => adgzl);
  ykl : entity work.kuinxx
    port map (yzdgrqy => qxvh, hntprmdpc => upagipkz, iinaxvpeda => adgzl);
  r : entity work.kuinxx
    port map (yzdgrqy => dauqwd, hntprmdpc => bbododh, iinaxvpeda => adgzl);
  
  -- Single-driven assignments
  cqeu <= cqeu;
  
  -- Multi-driven assignments
  qxvh <= qxvh;
  qxvh <= qxvh;
end xfkuq;

library ieee;
use ieee.std_logic_1164.all;

entity d is
  port (dlimz : buffer std_logic_vector(4 downto 0); oshmtofdn : in time);
end d;

architecture nlriulqx of d is
  signal yshq : real;
  signal vjvm : time;
begin
  glubu : entity work.qjqjgk
    port map (cqeu => vjvm, upagipkz => yshq);
  
  -- Multi-driven assignments
  dlimz <= "WULWZ";
  dlimz <= ('Z', 'H', 'W', 'X', '-');
  dlimz <= dlimz;
  dlimz <= "-ZXZL";
end nlriulqx;



-- Seed after: 14993398353859096474,5805648483995786113
