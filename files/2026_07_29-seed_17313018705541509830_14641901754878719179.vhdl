-- Seed: 17313018705541509830,14641901754878719179

entity vtfm is
  port (cqt : buffer boolean; kfmdvhrmk : linkage bit);
end vtfm;

architecture g of vtfm is
  
begin
  
end g;

library ieee;
use ieee.std_logic_1164.all;

entity fvnz is
  port (xavn : out time; v : in integer_vector(0 downto 1); huvkh : buffer std_logic_vector(0 to 4); unpgryqm : in time);
end fvnz;

architecture ahzvmixq of fvnz is
  signal limu : bit;
  signal el : boolean;
  signal apsvn : bit;
  signal rpiubakyb : boolean;
  signal dobwdl : bit;
  signal aiqitloq : boolean;
  signal vpkpspznbw : bit;
  signal bm : boolean;
begin
  tbq : entity work.vtfm
    port map (cqt => bm, kfmdvhrmk => vpkpspznbw);
  bl : entity work.vtfm
    port map (cqt => aiqitloq, kfmdvhrmk => dobwdl);
  byorupi : entity work.vtfm
    port map (cqt => rpiubakyb, kfmdvhrmk => apsvn);
  lcnzadigll : entity work.vtfm
    port map (cqt => el, kfmdvhrmk => limu);
  
  -- Single-driven assignments
  xavn <= unpgryqm;
  
  -- Multi-driven assignments
  huvkh <= ('W', 'U', 'W', '0', 'X');
  huvkh <= huvkh;
  huvkh <= ('U', '-', 'H', '0', 'Z');
end ahzvmixq;

entity ujty is
  port (fdxs : inout time; ceww : in real; fdivear : buffer real);
end ujty;

architecture fcbtw of ujty is
  signal nrcjy : bit;
  signal l : boolean;
  signal f : bit;
  signal mwojz : boolean;
  signal wth : bit;
  signal ukgnbyqaex : boolean;
  signal mcrdhf : bit;
  signal ugokmz : boolean;
begin
  j : entity work.vtfm
    port map (cqt => ugokmz, kfmdvhrmk => mcrdhf);
  fd : entity work.vtfm
    port map (cqt => ukgnbyqaex, kfmdvhrmk => wth);
  dnsznpffu : entity work.vtfm
    port map (cqt => mwojz, kfmdvhrmk => f);
  dbzfbkmojh : entity work.vtfm
    port map (cqt => l, kfmdvhrmk => nrcjy);
  
  -- Single-driven assignments
  fdxs <= 1 hr;
  fdivear <= 32131.4_3_2;
end fcbtw;

entity wwipqo is
  port (ducyhw : inout boolean_vector(2 to 3));
end wwipqo;

library ieee;
use ieee.std_logic_1164.all;

architecture c of wwipqo is
  signal wtwvjnch : std_logic_vector(0 to 4);
  signal zdum : integer_vector(0 downto 1);
  signal hxvvvhshi : time;
  signal pglnbvq : real;
  signal xpbbkbap : real;
  signal mbh : time;
  signal zefnhlzw : bit;
  signal bbnezlo : boolean;
begin
  y : entity work.vtfm
    port map (cqt => bbnezlo, kfmdvhrmk => zefnhlzw);
  eci : entity work.ujty
    port map (fdxs => mbh, ceww => xpbbkbap, fdivear => pglnbvq);
  rkpmuijasp : entity work.fvnz
    port map (xavn => hxvvvhshi, v => zdum, huvkh => wtwvjnch, unpgryqm => mbh);
  
  -- Single-driven assignments
  xpbbkbap <= xpbbkbap;
  zdum <= zdum;
  ducyhw <= ducyhw;
end c;



-- Seed after: 2661393235146198303,14641901754878719179
