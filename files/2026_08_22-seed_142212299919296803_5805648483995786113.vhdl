-- Seed: 142212299919296803,5805648483995786113

library ieee;
use ieee.std_logic_1164.all;

entity qgyggynrn is
  port (a : linkage std_logic; rjizcduz : inout time);
end qgyggynrn;

architecture ngnirm of qgyggynrn is
  
begin
  -- Single-driven assignments
  rjizcduz <= 3_0_0 ns;
end ngnirm;

entity fnp is
  port (sskhumatk : linkage real; rviccpc : in time; fg : out real_vector(0 downto 0));
end fnp;

library ieee;
use ieee.std_logic_1164.all;

architecture l of fnp is
  signal kfoaeolzg : time;
  signal s : time;
  signal bfnitgqyp : time;
  signal bwlvet : std_logic;
  signal orizr : time;
  signal rmuguodsx : std_logic;
begin
  ma : entity work.qgyggynrn
    port map (a => rmuguodsx, rjizcduz => orizr);
  vtdvsc : entity work.qgyggynrn
    port map (a => bwlvet, rjizcduz => bfnitgqyp);
  awq : entity work.qgyggynrn
    port map (a => rmuguodsx, rjizcduz => s);
  mbimpbo : entity work.qgyggynrn
    port map (a => rmuguodsx, rjizcduz => kfoaeolzg);
  
  -- Single-driven assignments
  fg <= fg;
  
  -- Multi-driven assignments
  bwlvet <= 'L';
  rmuguodsx <= '1';
  bwlvet <= rmuguodsx;
end l;

library ieee;
use ieee.std_logic_1164.all;

entity xewcajbvt is
  port (bk : in severity_level; tcqyend : linkage std_logic_vector(2 downto 3); lsqcqeio : buffer time);
end xewcajbvt;

library ieee;
use ieee.std_logic_1164.all;

architecture zghu of xewcajbvt is
  signal ycwhkzoa : std_logic;
begin
  mksx : entity work.qgyggynrn
    port map (a => ycwhkzoa, rjizcduz => lsqcqeio);
  
  -- Multi-driven assignments
  ycwhkzoa <= ycwhkzoa;
  ycwhkzoa <= 'Z';
  ycwhkzoa <= '-';
  ycwhkzoa <= 'H';
end zghu;

entity lclpynvm is
  port (kayhn : inout severity_level; g : buffer real);
end lclpynvm;

library ieee;
use ieee.std_logic_1164.all;

architecture ppv of lclpynvm is
  signal dmvsay : std_logic;
  signal txkxviq : real_vector(0 downto 0);
  signal oonv : real;
  signal fojmd : real_vector(0 downto 0);
  signal dej : time;
begin
  o : entity work.fnp
    port map (sskhumatk => g, rviccpc => dej, fg => fojmd);
  lzdxvf : entity work.fnp
    port map (sskhumatk => oonv, rviccpc => dej, fg => txkxviq);
  ma : entity work.qgyggynrn
    port map (a => dmvsay, rjizcduz => dej);
  
  -- Single-driven assignments
  kayhn <= FAILURE;
end ppv;



-- Seed after: 7567436998801788242,5805648483995786113
