-- Seed: 3462862742890886688,4080032123900078489

entity g is
  port (gidxv : buffer severity_level; cmejqjzq : linkage integer; jjfkpw : out real);
end g;

architecture ysgk of g is
  
begin
  -- Single-driven assignments
  jjfkpw <= jjfkpw;
  gidxv <= NOTE;
end ysgk;

entity bippxmc is
  port (xamhfc : linkage time);
end bippxmc;

architecture foqe of bippxmc is
  signal nnj : real;
  signal xfdstftyy : integer;
  signal dwjlpm : severity_level;
  signal jetadtia : real;
  signal djfzgrqta : integer;
  signal dw : severity_level;
begin
  asz : entity work.g
    port map (gidxv => dw, cmejqjzq => djfzgrqta, jjfkpw => jetadtia);
  wvarrw : entity work.g
    port map (gidxv => dwjlpm, cmejqjzq => xfdstftyy, jjfkpw => nnj);
end foqe;

entity eh is
  port (cqbgyvmp : linkage real_vector(1 to 2));
end eh;

architecture bvefc of eh is
  signal svqghr : real;
  signal unewdpl : integer;
  signal dzrql : severity_level;
begin
  cnmoqkrti : entity work.g
    port map (gidxv => dzrql, cmejqjzq => unewdpl, jjfkpw => svqghr);
end bvefc;

library ieee;
use ieee.std_logic_1164.all;

entity nbhrtdof is
  port (dy : buffer std_logic);
end nbhrtdof;

architecture ctnup of nbhrtdof is
  signal pxnsprwu : time;
  signal uzp : real_vector(1 to 2);
  signal mxosuqk : real;
  signal zfgav : integer;
  signal ok : severity_level;
  signal qnjuqr : real;
  signal zopexm : integer;
  signal ejtufye : severity_level;
begin
  fjqiva : entity work.g
    port map (gidxv => ejtufye, cmejqjzq => zopexm, jjfkpw => qnjuqr);
  e : entity work.g
    port map (gidxv => ok, cmejqjzq => zfgav, jjfkpw => mxosuqk);
  nbl : entity work.eh
    port map (cqbgyvmp => uzp);
  admzhkioc : entity work.bippxmc
    port map (xamhfc => pxnsprwu);
  
  -- Multi-driven assignments
  dy <= dy;
  dy <= dy;
  dy <= 'L';
  dy <= dy;
end ctnup;



-- Seed after: 5444366071919546354,4080032123900078489
