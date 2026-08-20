-- Seed: 9985416052774298401,499459191852795575

entity icb is
  port (uakozgse : buffer time; hsryb : out severity_level);
end icb;

architecture ajqstc of icb is
  
begin
  -- Single-driven assignments
  hsryb <= ERROR;
  uakozgse <= uakozgse;
end ajqstc;

library ieee;
use ieee.std_logic_1164.all;

entity q is
  port (ru : out std_logic; vuzwpkia : buffer std_logic; nidqng : inout integer);
end q;

architecture eaurxmzl of q is
  signal t : severity_level;
  signal wkverdxarx : time;
  signal qiyjdbs : severity_level;
  signal eqt : time;
  signal e : severity_level;
  signal wv : time;
  signal p : severity_level;
  signal r : time;
begin
  ngt : entity work.icb
    port map (uakozgse => r, hsryb => p);
  lwdby : entity work.icb
    port map (uakozgse => wv, hsryb => e);
  gqrcoc : entity work.icb
    port map (uakozgse => eqt, hsryb => qiyjdbs);
  tjyrzz : entity work.icb
    port map (uakozgse => wkverdxarx, hsryb => t);
  
  -- Multi-driven assignments
  vuzwpkia <= ru;
  vuzwpkia <= '0';
  vuzwpkia <= vuzwpkia;
end eaurxmzl;

entity deo is
  port (obtwextxyr : linkage real; sudkdf : in time; nm : buffer time);
end deo;

library ieee;
use ieee.std_logic_1164.all;

architecture cpge of deo is
  signal nbpalse : integer;
  signal cpnp : integer;
  signal zjmfbt : std_logic;
  signal duncwbhor : std_logic;
  signal kopjemo : severity_level;
  signal qwblcs : time;
  signal ksmjnsjlw : integer;
  signal gzufe : std_logic;
begin
  ez : entity work.q
    port map (ru => gzufe, vuzwpkia => gzufe, nidqng => ksmjnsjlw);
  cuorcfyc : entity work.icb
    port map (uakozgse => qwblcs, hsryb => kopjemo);
  koixxqspb : entity work.q
    port map (ru => duncwbhor, vuzwpkia => zjmfbt, nidqng => cpnp);
  rmqucejbl : entity work.q
    port map (ru => gzufe, vuzwpkia => gzufe, nidqng => nbpalse);
  
  -- Single-driven assignments
  nm <= 16#402.D_C_5# fs;
  
  -- Multi-driven assignments
  zjmfbt <= 'Z';
  zjmfbt <= gzufe;
  gzufe <= '0';
  gzufe <= 'L';
end cpge;

entity k is
  port (beztogvo : buffer time; pbttnlztb : out real; mipldzg : linkage real);
end k;

library ieee;
use ieee.std_logic_1164.all;

architecture bcwa of k is
  signal lqgqf : time;
  signal zjoyapp : severity_level;
  signal znz : severity_level;
  signal bftyxtpjda : time;
  signal xibewhetjw : integer;
  signal ofm : std_logic;
  signal koxaromyaa : std_logic;
begin
  kprezlvuxm : entity work.q
    port map (ru => koxaromyaa, vuzwpkia => ofm, nidqng => xibewhetjw);
  vbxewcyl : entity work.icb
    port map (uakozgse => bftyxtpjda, hsryb => znz);
  vsjclw : entity work.icb
    port map (uakozgse => beztogvo, hsryb => zjoyapp);
  ftnvyoqzj : entity work.deo
    port map (obtwextxyr => pbttnlztb, sudkdf => lqgqf, nm => lqgqf);
  
  -- Multi-driven assignments
  koxaromyaa <= koxaromyaa;
  koxaromyaa <= koxaromyaa;
  koxaromyaa <= 'Z';
end bcwa;



-- Seed after: 4408727689620738268,499459191852795575
