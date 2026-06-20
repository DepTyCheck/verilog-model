-- Seed: 5808183721998725011,3924983747739634027

library ieee;
use ieee.std_logic_1164.all;

entity nxkgo is
  port (ek : inout std_logic; b : in severity_level; zjyoxbree : buffer std_logic_vector(1 to 4); obqgxztfmy : linkage time);
end nxkgo;

architecture zsjot of nxkgo is
  
begin
  -- Multi-driven assignments
  ek <= 'X';
end zsjot;

library ieee;
use ieee.std_logic_1164.all;

entity wgeuauv is
  port (uyhsi : in std_logic; fchymvukf : in time; dfudk : out real);
end wgeuauv;

library ieee;
use ieee.std_logic_1164.all;

architecture rwyp of wgeuauv is
  signal yx : time;
  signal wamhngupj : time;
  signal rjdj : time;
  signal bj : std_logic_vector(1 to 4);
  signal nurjimtai : time;
  signal yas : std_logic_vector(1 to 4);
  signal nzca : severity_level;
  signal r : std_logic;
begin
  qs : entity work.nxkgo
    port map (ek => r, b => nzca, zjyoxbree => yas, obqgxztfmy => nurjimtai);
  zjjwaak : entity work.nxkgo
    port map (ek => r, b => nzca, zjyoxbree => bj, obqgxztfmy => rjdj);
  zf : entity work.nxkgo
    port map (ek => r, b => nzca, zjyoxbree => yas, obqgxztfmy => wamhngupj);
  pcc : entity work.nxkgo
    port map (ek => r, b => nzca, zjyoxbree => bj, obqgxztfmy => yx);
  
  -- Single-driven assignments
  dfudk <= 2#011.11#;
  nzca <= ERROR;
  
  -- Multi-driven assignments
  r <= 'H';
  r <= 'U';
end rwyp;

entity a is
  port (ebnbecikh : out integer);
end a;

library ieee;
use ieee.std_logic_1164.all;

architecture l of a is
  signal lgupujwgpf : real;
  signal gpjs : time;
  signal quyzklli : std_logic;
begin
  gcauopobg : entity work.wgeuauv
    port map (uyhsi => quyzklli, fchymvukf => gpjs, dfudk => lgupujwgpf);
  
  -- Single-driven assignments
  gpjs <= 2 min;
  ebnbecikh <= 16#5#;
  
  -- Multi-driven assignments
  quyzklli <= '1';
  quyzklli <= 'X';
  quyzklli <= '-';
end l;

entity pu is
  port (jhk : inout time);
end pu;

library ieee;
use ieee.std_logic_1164.all;

architecture wvklrdqw of pu is
  signal apxollm : real;
  signal voo : time;
  signal sdqocy : severity_level;
  signal smuuxyo : time;
  signal svcyf : std_logic_vector(1 to 4);
  signal celtydju : severity_level;
  signal lpa : time;
  signal sfqrvxijp : std_logic_vector(1 to 4);
  signal nuzvua : severity_level;
  signal dsfbrhno : std_logic;
begin
  ofakjszdla : entity work.nxkgo
    port map (ek => dsfbrhno, b => nuzvua, zjyoxbree => sfqrvxijp, obqgxztfmy => lpa);
  uq : entity work.nxkgo
    port map (ek => dsfbrhno, b => celtydju, zjyoxbree => svcyf, obqgxztfmy => smuuxyo);
  olnkh : entity work.nxkgo
    port map (ek => dsfbrhno, b => sdqocy, zjyoxbree => sfqrvxijp, obqgxztfmy => jhk);
  lvrgojncmn : entity work.wgeuauv
    port map (uyhsi => dsfbrhno, fchymvukf => voo, dfudk => apxollm);
  
  -- Multi-driven assignments
  dsfbrhno <= 'Z';
  dsfbrhno <= 'L';
  sfqrvxijp <= ('-', '1', 'X', 'W');
end wvklrdqw;



-- Seed after: 15886492343156852222,3924983747739634027
