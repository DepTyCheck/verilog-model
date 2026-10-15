-- Seed: 16014804961134137232,6377581586043967497

entity lnjdn is
  port (pjk : buffer bit; l : inout boolean; zzycn : inout string(5 to 3));
end lnjdn;

architecture yaocl of lnjdn is
  
begin
  -- Single-driven assignments
  zzycn <= "";
  pjk <= pjk;
  l <= FALSE;
end yaocl;

entity wlfsrxrcg is
  port (svpblbo : in bit; mfoyzun : inout string(2 downto 2));
end wlfsrxrcg;

architecture nei of wlfsrxrcg is
  signal r : string(5 to 3);
  signal w : boolean;
  signal dlaqqwhrp : bit;
  signal pmptc : string(5 to 3);
  signal ovrgmersb : boolean;
  signal rmuyyjh : bit;
  signal hlowcxijsc : string(5 to 3);
  signal rcrqkrzdig : boolean;
  signal si : bit;
begin
  ysyk : entity work.lnjdn
    port map (pjk => si, l => rcrqkrzdig, zzycn => hlowcxijsc);
  qscza : entity work.lnjdn
    port map (pjk => rmuyyjh, l => ovrgmersb, zzycn => pmptc);
  ypgz : entity work.lnjdn
    port map (pjk => dlaqqwhrp, l => w, zzycn => r);
  
  -- Single-driven assignments
  mfoyzun <= mfoyzun;
end nei;



-- Seed after: 13120588934949985417,6377581586043967497
