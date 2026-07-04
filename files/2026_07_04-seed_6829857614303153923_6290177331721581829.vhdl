-- Seed: 6829857614303153923,6290177331721581829

use std.reflection.all;

entity wuhkaaodj is
  port (ioeden : inout physical_subtype_mirror; nq : inout protected_value_mirror; tdbhn : in bit);
end wuhkaaodj;

architecture p of wuhkaaodj is
  
begin
  
end p;

use std.reflection.all;

entity ixxj is
  port ( rvlbdtps : inout integer_value_mirror
  ; qkppsejg : inout array_subtype_mirror
  ; hnlw : inout array_value_mirror
  ; gikumak : inout floating_value_mirror
  );
end ixxj;

use std.reflection.all;

architecture bmeeywap of ixxj is
  signal qf : bit;
  shared variable upg : protected_value_mirror;
  shared variable qxvuhcpgq : physical_subtype_mirror;
  signal blgjqdp : bit;
  shared variable loqrwdbkpc : protected_value_mirror;
  shared variable csolfhbitm : physical_subtype_mirror;
begin
  vgeen : entity work.wuhkaaodj
    port map (ioeden => csolfhbitm, nq => loqrwdbkpc, tdbhn => blgjqdp);
  kl : entity work.wuhkaaodj
    port map (ioeden => qxvuhcpgq, nq => upg, tdbhn => qf);
  
  -- Single-driven assignments
  blgjqdp <= '1';
  qf <= qf;
end bmeeywap;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity ynthh is
  port (n : buffer real_vector(0 to 2); xbqaxfvvy : inout floating_subtype_mirror; wvaxksues : inout record_value_mirror; kfhpm : in std_logic);
end ynthh;

architecture q of ynthh is
  
begin
  -- Single-driven assignments
  n <= n;
end q;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity kemu is
  port (amvfkia : inout array_value_mirror; kmy : inout physical_value_mirror; eptwadl : buffer std_logic_vector(3 downto 4));
end kemu;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture qfpk of kemu is
  signal nydx : std_logic;
  shared variable shfdupx : record_value_mirror;
  shared variable itnqtjv : floating_subtype_mirror;
  signal kiqxd : real_vector(0 to 2);
  shared variable fmzqaa : protected_value_mirror;
  shared variable pwlxjukhqv : physical_subtype_mirror;
  signal c : bit;
  shared variable azxfqpri : protected_value_mirror;
  shared variable tqfjwivtae : physical_subtype_mirror;
  signal caf : bit;
  shared variable rcoavbf : protected_value_mirror;
  shared variable a : physical_subtype_mirror;
begin
  nhtyyz : entity work.wuhkaaodj
    port map (ioeden => a, nq => rcoavbf, tdbhn => caf);
  cnmqmywenc : entity work.wuhkaaodj
    port map (ioeden => tqfjwivtae, nq => azxfqpri, tdbhn => c);
  rba : entity work.wuhkaaodj
    port map (ioeden => pwlxjukhqv, nq => fmzqaa, tdbhn => caf);
  fzsh : entity work.ynthh
    port map (n => kiqxd, xbqaxfvvy => itnqtjv, wvaxksues => shfdupx, kfhpm => nydx);
  
  -- Multi-driven assignments
  eptwadl <= (others => '0');
  eptwadl <= (others => '0');
  eptwadl <= "";
end qfpk;



-- Seed after: 12881944028378395951,6290177331721581829
