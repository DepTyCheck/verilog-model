-- Seed: 13601687445979618512,2983771601630957889

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity dsua is
  port ( n : buffer time
  ; variable vq : inout floating_value_mirror_pt
  ; bqtpvi : in std_logic_vector(0 downto 0)
  ; variable trcgfypov : inout subtype_mirror_pt
  );
end dsua;

architecture pxyvrncuf of dsua is
  
begin
  -- Single-driven assignments
  n <= 203 fs;
end pxyvrncuf;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity apijm is
  port ( variable usgdfbrk : inout array_subtype_mirror_pt
  ; cwofwftco : buffer std_logic
  ; variable sfpfxryqh : inout access_subtype_mirror_pt
  ; variable wifegd : inout protected_value_mirror_pt
  );
end apijm;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture tcreou of apijm is
  shared variable wszclgc : subtype_mirror_pt;
  signal lpenpl : std_logic_vector(0 downto 0);
  shared variable wbsppr : floating_value_mirror_pt;
  signal ksj : time;
  shared variable nrrmikyyx : subtype_mirror_pt;
  shared variable y : floating_value_mirror_pt;
  signal nzwjtsxe : time;
  shared variable etixj : subtype_mirror_pt;
  signal ftdyo : std_logic_vector(0 downto 0);
  shared variable wzjmb : floating_value_mirror_pt;
  signal vhvpo : time;
  shared variable cjqqqao : subtype_mirror_pt;
  signal bxpkzg : std_logic_vector(0 downto 0);
  shared variable ssgqlkobkt : floating_value_mirror_pt;
  signal d : time;
begin
  xzrtjdo : entity work.dsua
    port map (n => d, vq => ssgqlkobkt, bqtpvi => bxpkzg, trcgfypov => cjqqqao);
  k : entity work.dsua
    port map (n => vhvpo, vq => wzjmb, bqtpvi => ftdyo, trcgfypov => etixj);
  mzggqmotf : entity work.dsua
    port map (n => nzwjtsxe, vq => y, bqtpvi => bxpkzg, trcgfypov => nrrmikyyx);
  hnltexe : entity work.dsua
    port map (n => ksj, vq => wbsppr, bqtpvi => lpenpl, trcgfypov => wszclgc);
  
  -- Multi-driven assignments
  cwofwftco <= 'W';
  lpenpl <= bxpkzg;
  bxpkzg <= "-";
  bxpkzg <= lpenpl;
end tcreou;

use std.reflection.all;

entity fwmi is
  port (variable h : inout floating_subtype_mirror_pt; variable vgqqk : inout protected_value_mirror_pt);
end fwmi;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture xyifdazayd of fwmi is
  shared variable d : subtype_mirror_pt;
  signal b : std_logic_vector(0 downto 0);
  shared variable w : floating_value_mirror_pt;
  signal dkpvlvjcqa : time;
  shared variable hwuuyh : access_subtype_mirror_pt;
  signal pvjzgkcn : std_logic;
  shared variable vtf : array_subtype_mirror_pt;
begin
  wvkfaagp : entity work.apijm
    port map (usgdfbrk => vtf, cwofwftco => pvjzgkcn, sfpfxryqh => hwuuyh, wifegd => vgqqk);
  opnfzkd : entity work.dsua
    port map (n => dkpvlvjcqa, vq => w, bqtpvi => b, trcgfypov => d);
end xyifdazayd;

use std.reflection.all;

entity lbawvbx is
  port (variable c : inout record_value_mirror_pt; bjwjtmvvht : buffer real);
end lbawvbx;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture lm of lbawvbx is
  shared variable qa : subtype_mirror_pt;
  shared variable uyekhxaas : floating_value_mirror_pt;
  signal vezh : time;
  shared variable aeflfh : subtype_mirror_pt;
  signal vwxdjr : std_logic_vector(0 downto 0);
  shared variable mkwrm : floating_value_mirror_pt;
  signal fxvctcqd : time;
  shared variable cju : protected_value_mirror_pt;
  shared variable azbrnhgc : access_subtype_mirror_pt;
  signal ttdfblywlr : std_logic;
  shared variable czf : array_subtype_mirror_pt;
begin
  z : entity work.apijm
    port map (usgdfbrk => czf, cwofwftco => ttdfblywlr, sfpfxryqh => azbrnhgc, wifegd => cju);
  tnesasma : entity work.dsua
    port map (n => fxvctcqd, vq => mkwrm, bqtpvi => vwxdjr, trcgfypov => aeflfh);
  brllz : entity work.dsua
    port map (n => vezh, vq => uyekhxaas, bqtpvi => vwxdjr, trcgfypov => qa);
  
  -- Multi-driven assignments
  ttdfblywlr <= ttdfblywlr;
end lm;



-- Seed after: 18063320542238069407,2983771601630957889
