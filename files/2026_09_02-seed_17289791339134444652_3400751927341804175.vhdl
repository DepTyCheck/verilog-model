-- Seed: 17289791339134444652,3400751927341804175

entity lzkpoozp is
  port (tihangmfsn : buffer time; mhzkmrl : buffer integer; fpck : out time; ljzm : out time);
end lzkpoozp;

architecture jr of lzkpoozp is
  
begin
  -- Single-driven assignments
  ljzm <= ljzm;
  tihangmfsn <= fpck;
  mhzkmrl <= mhzkmrl;
  fpck <= 1 hr;
end jr;

entity gvshgs is
  port (tisu : out bit_vector(2 downto 3));
end gvshgs;

architecture ajsntuxmcv of gvshgs is
  
begin
  -- Single-driven assignments
  tisu <= (others => '0');
end ajsntuxmcv;

library ieee;
use ieee.std_logic_1164.all;

entity dqjg is
  port (rqocdzg : inout time_vector(1 downto 3); fjzadvmgrb : in time; ygxjvi : out std_logic);
end dqjg;

architecture parcjc of dqjg is
  signal ibx : time;
  signal vfxnt : time;
  signal jctquzraph : integer;
  signal hx : time;
  signal qtgqw : time;
  signal uykxexna : time;
  signal ognjt : integer;
  signal aykqiojzxc : time;
begin
  l : entity work.lzkpoozp
    port map (tihangmfsn => aykqiojzxc, mhzkmrl => ognjt, fpck => uykxexna, ljzm => qtgqw);
  kgzvrcu : entity work.lzkpoozp
    port map (tihangmfsn => hx, mhzkmrl => jctquzraph, fpck => vfxnt, ljzm => ibx);
  
  -- Single-driven assignments
  rqocdzg <= (others => 0 ns);
  
  -- Multi-driven assignments
  ygxjvi <= 'L';
end parcjc;

entity wpinmfmcxo is
  port (iazrpg : inout integer; i : inout time; juo : inout time);
end wpinmfmcxo;

architecture eqr of wpinmfmcxo is
  signal xltwwqum : time;
  signal opxkkz : time;
  signal mggrpbcu : integer;
  signal gmc : time;
  signal qdj : time;
  signal gq : time;
  signal ijkm : time;
  signal urlyangc : integer;
  signal xvfn : time;
begin
  si : entity work.lzkpoozp
    port map (tihangmfsn => xvfn, mhzkmrl => urlyangc, fpck => ijkm, ljzm => gq);
  evco : entity work.lzkpoozp
    port map (tihangmfsn => qdj, mhzkmrl => iazrpg, fpck => gmc, ljzm => i);
  gx : entity work.lzkpoozp
    port map (tihangmfsn => juo, mhzkmrl => mggrpbcu, fpck => opxkkz, ljzm => xltwwqum);
end eqr;



-- Seed after: 5043849762452306560,3400751927341804175
