-- Seed: 10272182469018128525,5906004015519833893

entity pljcxldyp is
  port (nk : buffer time; qy : in real; nl : in time);
end pljcxldyp;

architecture a of pljcxldyp is
  
begin
  -- Single-driven assignments
  nk <= nl;
end a;

library ieee;
use ieee.std_logic_1164.all;

entity j is
  port (eeygt : inout std_logic_vector(4 downto 0));
end j;

architecture uhnttgaw of j is
  signal ugfdoi : time;
  signal kdclf : real;
  signal aopwib : time;
  signal ksgwtge : time;
  signal opscxxai : real;
  signal eogsxpge : time;
  signal lhdtvyxqv : time;
  signal yetvab : real;
  signal pjdeye : time;
begin
  akrxacqg : entity work.pljcxldyp
    port map (nk => pjdeye, qy => yetvab, nl => lhdtvyxqv);
  vfpifob : entity work.pljcxldyp
    port map (nk => eogsxpge, qy => opscxxai, nl => ksgwtge);
  uzdvyw : entity work.pljcxldyp
    port map (nk => aopwib, qy => kdclf, nl => ugfdoi);
  
  -- Single-driven assignments
  lhdtvyxqv <= ksgwtge;
  ugfdoi <= 1 hr;
  ksgwtge <= pjdeye;
  opscxxai <= kdclf;
  yetvab <= yetvab;
  
  -- Multi-driven assignments
  eeygt <= ('U', '-', '0', '-', 'U');
  eeygt <= "H0000";
  eeygt <= ('H', '-', 'H', '0', 'U');
  eeygt <= eeygt;
end uhnttgaw;

library ieee;
use ieee.std_logic_1164.all;

entity vn is
  port (hdf : out time; zkxhhi : in std_logic_vector(4 to 1); g : linkage time; lmgurkl : buffer boolean_vector(2 to 2));
end vn;

architecture f of vn is
  signal zqelgsojco : real;
  signal wobgqqfuo : time;
begin
  xdrzbwnu : entity work.pljcxldyp
    port map (nk => wobgqqfuo, qy => zqelgsojco, nl => wobgqqfuo);
  z : entity work.pljcxldyp
    port map (nk => hdf, qy => zqelgsojco, nl => wobgqqfuo);
  
  -- Single-driven assignments
  zqelgsojco <= 2#1.0#;
end f;



-- Seed after: 11211159308071809151,5906004015519833893
