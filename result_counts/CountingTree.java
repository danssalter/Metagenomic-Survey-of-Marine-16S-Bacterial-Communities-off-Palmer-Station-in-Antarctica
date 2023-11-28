//package palmer.sixteenS;

// Author: Dr. Philip Heller

import java.io.*;
import java.util.*;
import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.tree.DefaultMutableTreeNode;


public class CountingTree 
{
	Node					root;
	DefaultMutableTreeNode	rootDMTN;
	
	
	CountingTree(File tsv) throws IOException
	{
		root = new Node("Bacteria");
		
		// Build down from root. At first only leaf nodes have counts.
		try (FileReader fr = new FileReader(tsv); BufferedReader br = new BufferedReader(fr))
		{
			br.readLine();
			String line;
			while ((line = br.readLine()) != null)
			{
				String[] taxoPieces = line.split("\\t")[2].split(";");
				Node parent = root;
				Node kid = null;
				for (int i=1; i<taxoPieces.length; i++)
				{
					String taxoPiece = taxoPieces[i];	// "Flavobacteriales"(100) or Flavobacteriales(100)
					taxoPiece = taxoPiece.replaceAll("\"", "");
					taxoPiece = taxoPiece.substring(0, taxoPiece.lastIndexOf('('));
					kid = parent.getKidWithName(taxoPiece);
					if (kid == null)
					{
						kid = new Node(taxoPiece);
						parent.add(kid);
					}
					parent = kid;
				}
				long count = Long.valueOf(line.split("\\t")[1]);
				kid.count += count;    
			}
		}
		
		// Propagate counts.
		root.computeCountRecurse();

		// Convert to Swing.
		rootDMTN = root.toDMTNode();
	}
	
	
	
	
	
	private class Node
	{
		Node				parent;
		ArrayList<Node> 	kids;
		String				name;
		Long				count;
		
		Node(String name)
		{
			this.name = name;
			this.count = 0L; 	
		}
		
		Node getKidWithName(String name)
		{
			if (kids == null)
				return null;
			for (Node kid: kids)
				if (kid.name.equals(name))
					return kid;
			return null;
		}
		
		void add(Node kid)
		{
			if (kids == null)
				kids = new ArrayList<>();
			kids.add(kid);
			kid.parent = this;
		}
		
		long computeCountRecurse()
		{
			if (kids == null)
				return count;
			else
			{
				long ret = 0L;
				for (Node kid: kids)
					ret += kid.computeCountRecurse();
				count = ret;
				return ret;
			}
		}
		
		DefaultMutableTreeNode toDMTNode()
		{
			DefaultMutableTreeNode ret = new DefaultMutableTreeNode(this, kids != null);
			if (kids != null)
			{
				for (Node kid: kids)
				{
					DefaultMutableTreeNode kidNode = kid.toDMTNode();
					ret.add(kidNode);
				}
			}
			return ret;
		}
		
		@Override
		public String toString()
		{
			return name + ": " + count;
		}
	}  // Node
	
	
	static class TreeFrame extends JFrame implements ActionListener
	{
		private JButton				quitBtn;
		private JComboBox<File> 	combo;
		private JScrollPane			spane;
		
		TreeFrame(File dirf) throws IOException
		{
			JPanel pan = new JPanel();			
			Vector<File> tsvs = new Vector<>();
			for (String kid: dirf.list())
			{
				if (kid.endsWith(".taxonomy"))
				{
					File tsv = new File(dirf, kid);
					tsvs.add(tsv);	
				}
			}
			combo = new JComboBox<>(tsvs);
			combo.addActionListener(this);
			pan.add(combo);
			quitBtn = new JButton("Quit");
			quitBtn.addActionListener(this);
			pan.add(quitBtn);
			add(pan, BorderLayout.NORTH);
			
			CountingTree countingTree = new CountingTree((File)combo.getSelectedItem());
			JTree jtree = new JTree(countingTree.rootDMTN);
			spane = new JScrollPane(jtree, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS, JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
			spane.setPreferredSize(new Dimension(230, 550));
			add(spane, BorderLayout.CENTER);
			pack();
		}

		@Override
		public void actionPerformed(ActionEvent e) 
		{
			if (e.getSource() == quitBtn)
				System.exit(1);
			
			else if (e.getSource() == combo)
			{
				try
				{
					CountingTree countingTree = new CountingTree((File)combo.getSelectedItem());
					spane.setViewportView(new JTree(countingTree.rootDMTN));
				}
				catch (IOException x)
				{
					sop("STRESS: " + x.getMessage());
					x.printStackTrace();
				}
			}
		}
	}
	
	
	static void sop(Object x) 	{ System.out.println(x); }
	
	
	public static void main(String[] args) throws Exception
	{
		sop("START");
		if (args.length == 0)
			args = new String[] { "data/16S_Daniel/result_counts" };
		File dirf = new File(args[0]);
		new TreeFrame(dirf).setVisible(true);
		sop("DONE");
	}
}
