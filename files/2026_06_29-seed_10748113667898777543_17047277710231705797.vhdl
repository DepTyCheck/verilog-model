-- Seed: 10748113667898777543,17047277710231705797

library ieee;
use ieee.std_logic_1164.all;

entity hf is
  port (cmfokeqtp : in std_logic_vector(3 downto 0); kzf : buffer boolean; vqb : in real);
end hf;

architecture csgathay of hf is
  
begin
  -- Single-driven assignments
  kzf <= TRUE;
end csgathay;

entity r is
  port (qoaavsdmix : inout real; pdgpejrl : buffer bit);
end r;

library ieee;
use ieee.std_logic_1164.all;

architecture neuuxjbgt of r is
  signal hrmjmiy : real;
  signal udfvqrem : boolean;
  signal a : real;
  signal gxgnv : boolean;
  signal w : std_logic_vector(3 downto 0);
  signal fwbyu : real;
  signal qqtpjz : boolean;
  signal sd : std_logic_vector(3 downto 0);
begin
  uavhu : entity work.hf
    port map (cmfokeqtp => sd, kzf => qqtpjz, vqb => fwbyu);
  ytrggkov : entity work.hf
    port map (cmfokeqtp => w, kzf => gxgnv, vqb => a);
  quaurdo : entity work.hf
    port map (cmfokeqtp => w, kzf => udfvqrem, vqb => hrmjmiy);
  
  -- Single-driven assignments
  pdgpejrl <= '1';
  qoaavsdmix <= 2#0.11#;
  
  -- Multi-driven assignments
  sd <= ('0', 'X', 'W', 'H');
  sd <= ('H', 'X', 'X', '-');
end neuuxjbgt;

entity i is
  port (rggfzvyc : inout time; p : buffer integer; o : inout real; lcwwpgcb : buffer integer);
end i;

architecture lvszsn of i is
  signal cqj : bit;
begin
  fjfdtfnjd : entity work.r
    port map (qoaavsdmix => o, pdgpejrl => cqj);
  
  -- Single-driven assignments
  lcwwpgcb <= 0431;
  p <= 0_3_4_0_1;
  rggfzvyc <= 8#365.202# us;
end lvszsn;

entity avrqfuxmgg is
  port (nfw : buffer real; eh : buffer integer);
end avrqfuxmgg;

library ieee;
use ieee.std_logic_1164.all;

architecture ygvxbuetqn of avrqfuxmgg is
  signal nlkxkkuf : integer;
  signal tvyn : real;
  signal ibdsqwczag : time;
  signal uosdufrmt : boolean;
  signal jaydh : std_logic_vector(3 downto 0);
  signal olvczpovp : bit;
  signal ncigm : integer;
  signal e : real;
  signal amtxxislq : integer;
  signal allipzhge : time;
begin
  pcmgcfst : entity work.i
    port map (rggfzvyc => allipzhge, p => amtxxislq, o => e, lcwwpgcb => ncigm);
  wwgqkzz : entity work.r
    port map (qoaavsdmix => nfw, pdgpejrl => olvczpovp);
  rvt : entity work.hf
    port map (cmfokeqtp => jaydh, kzf => uosdufrmt, vqb => nfw);
  kuzxx : entity work.i
    port map (rggfzvyc => ibdsqwczag, p => eh, o => tvyn, lcwwpgcb => nlkxkkuf);
  
  -- Multi-driven assignments
  jaydh <= ('1', 'Z', 'X', '1');
  jaydh <= ('L', '1', 'H', '1');
end ygvxbuetqn;



-- Seed after: 13979557138546950506,17047277710231705797
