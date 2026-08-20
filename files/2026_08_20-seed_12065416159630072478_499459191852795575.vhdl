-- Seed: 12065416159630072478,499459191852795575

entity punxkxoqdw is
  port (owhmzpsn : buffer time; vwlupl : inout integer; ibivw : out time_vector(1 downto 4));
end punxkxoqdw;

architecture qbor of punxkxoqdw is
  
begin
  -- Single-driven assignments
  owhmzpsn <= 16#6_D_7_F_C# ms;
  vwlupl <= 16#BB74C#;
  ibivw <= (others => 0 ns);
end qbor;

library ieee;
use ieee.std_logic_1164.all;

entity e is
  port (flhsnrpzjn : buffer std_logic_vector(1 to 1); fcwc : inout time; atk : out std_logic_vector(2 to 0));
end e;

architecture lyysgy of e is
  signal fjg : time_vector(1 downto 4);
  signal pnm : integer;
  signal ywuh : time;
  signal lqxesbh : time_vector(1 downto 4);
  signal h : integer;
  signal sfgpwy : time_vector(1 downto 4);
  signal gbsa : integer;
  signal rn : time;
  signal azguc : time_vector(1 downto 4);
  signal yex : integer;
  signal ffog : time;
begin
  gr : entity work.punxkxoqdw
    port map (owhmzpsn => ffog, vwlupl => yex, ibivw => azguc);
  sd : entity work.punxkxoqdw
    port map (owhmzpsn => rn, vwlupl => gbsa, ibivw => sfgpwy);
  udfw : entity work.punxkxoqdw
    port map (owhmzpsn => fcwc, vwlupl => h, ibivw => lqxesbh);
  jpg : entity work.punxkxoqdw
    port map (owhmzpsn => ywuh, vwlupl => pnm, ibivw => fjg);
  
  -- Multi-driven assignments
  atk <= atk;
  atk <= atk;
  atk <= atk;
  atk <= atk;
end lyysgy;



-- Seed after: 10393120267907005558,499459191852795575
