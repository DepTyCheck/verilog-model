-- Seed: 17075910960410550316,8581541265164261811



entity bsn is
  port (blncsw : buffer integer);
end bsn;



architecture rxb of bsn is
  
begin
  
end rxb;



entity uqe is
  port (vqgen : inout integer);
end uqe;



architecture zvocqlty of uqe is
  
begin
  qau : entity work.bsn
    port map (blncsw => vqgen);
end zvocqlty;



-- Seed after: 11848426146921857536,8581541265164261811
