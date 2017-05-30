
(function() {
'use strict';

function F2(fun)
{
  function wrapper(a) { return function(b) { return fun(a,b); }; }
  wrapper.arity = 2;
  wrapper.func = fun;
  return wrapper;
}

function F3(fun)
{
  function wrapper(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  }
  wrapper.arity = 3;
  wrapper.func = fun;
  return wrapper;
}

function F4(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  }
  wrapper.arity = 4;
  wrapper.func = fun;
  return wrapper;
}

function F5(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  }
  wrapper.arity = 5;
  wrapper.func = fun;
  return wrapper;
}

function F6(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  }
  wrapper.arity = 6;
  wrapper.func = fun;
  return wrapper;
}

function F7(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  }
  wrapper.arity = 7;
  wrapper.func = fun;
  return wrapper;
}

function F8(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  }
  wrapper.arity = 8;
  wrapper.func = fun;
  return wrapper;
}

function F9(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  }
  wrapper.arity = 9;
  wrapper.func = fun;
  return wrapper;
}

function A2(fun, a, b)
{
  return fun.arity === 2
    ? fun.func(a, b)
    : fun(a)(b);
}
function A3(fun, a, b, c)
{
  return fun.arity === 3
    ? fun.func(a, b, c)
    : fun(a)(b)(c);
}
function A4(fun, a, b, c, d)
{
  return fun.arity === 4
    ? fun.func(a, b, c, d)
    : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e)
{
  return fun.arity === 5
    ? fun.func(a, b, c, d, e)
    : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f)
{
  return fun.arity === 6
    ? fun.func(a, b, c, d, e, f)
    : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g)
{
  return fun.arity === 7
    ? fun.func(a, b, c, d, e, f, g)
    : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h)
{
  return fun.arity === 8
    ? fun.func(a, b, c, d, e, f, g, h)
    : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i)
{
  return fun.arity === 9
    ? fun.func(a, b, c, d, e, f, g, h, i)
    : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

//import Native.List //

var _elm_lang$core$Native_Array = function() {

// A RRB-Tree has two distinct data types.
// Leaf -> "height"  is always 0
//         "table"   is an array of elements
// Node -> "height"  is always greater than 0
//         "table"   is an array of child nodes
//         "lengths" is an array of accumulated lengths of the child nodes

// M is the maximal table size. 32 seems fast. E is the allowed increase
// of search steps when concatting to find an index. Lower values will
// decrease balancing, but will increase search steps.
var M = 32;
var E = 2;

// An empty array.
var empty = {
	ctor: '_Array',
	height: 0,
	table: []
};


function get(i, array)
{
	if (i < 0 || i >= length(array))
	{
		throw new Error(
			'Index ' + i + ' is out of range. Check the length of ' +
			'your array first or use getMaybe or getWithDefault.');
	}
	return unsafeGet(i, array);
}


function unsafeGet(i, array)
{
	for (var x = array.height; x > 0; x--)
	{
		var slot = i >> (x * 5);
		while (array.lengths[slot] <= i)
		{
			slot++;
		}
		if (slot > 0)
		{
			i -= array.lengths[slot - 1];
		}
		array = array.table[slot];
	}
	return array.table[i];
}


// Sets the value at the index i. Only the nodes leading to i will get
// copied and updated.
function set(i, item, array)
{
	if (i < 0 || length(array) <= i)
	{
		return array;
	}
	return unsafeSet(i, item, array);
}


function unsafeSet(i, item, array)
{
	array = nodeCopy(array);

	if (array.height === 0)
	{
		array.table[i] = item;
	}
	else
	{
		var slot = getSlot(i, array);
		if (slot > 0)
		{
			i -= array.lengths[slot - 1];
		}
		array.table[slot] = unsafeSet(i, item, array.table[slot]);
	}
	return array;
}


function initialize(len, f)
{
	if (len <= 0)
	{
		return empty;
	}
	var h = Math.floor( Math.log(len) / Math.log(M) );
	return initialize_(f, h, 0, len);
}

function initialize_(f, h, from, to)
{
	if (h === 0)
	{
		var table = new Array((to - from) % (M + 1));
		for (var i = 0; i < table.length; i++)
		{
		  table[i] = f(from + i);
		}
		return {
			ctor: '_Array',
			height: 0,
			table: table
		};
	}

	var step = Math.pow(M, h);
	var table = new Array(Math.ceil((to - from) / step));
	var lengths = new Array(table.length);
	for (var i = 0; i < table.length; i++)
	{
		table[i] = initialize_(f, h - 1, from + (i * step), Math.min(from + ((i + 1) * step), to));
		lengths[i] = length(table[i]) + (i > 0 ? lengths[i-1] : 0);
	}
	return {
		ctor: '_Array',
		height: h,
		table: table,
		lengths: lengths
	};
}

function fromList(list)
{
	if (list.ctor === '[]')
	{
		return empty;
	}

	// Allocate M sized blocks (table) and write list elements to it.
	var table = new Array(M);
	var nodes = [];
	var i = 0;

	while (list.ctor !== '[]')
	{
		table[i] = list._0;
		list = list._1;
		i++;

		// table is full, so we can push a leaf containing it into the
		// next node.
		if (i === M)
		{
			var leaf = {
				ctor: '_Array',
				height: 0,
				table: table
			};
			fromListPush(leaf, nodes);
			table = new Array(M);
			i = 0;
		}
	}

	// Maybe there is something left on the table.
	if (i > 0)
	{
		var leaf = {
			ctor: '_Array',
			height: 0,
			table: table.splice(0, i)
		};
		fromListPush(leaf, nodes);
	}

	// Go through all of the nodes and eventually push them into higher nodes.
	for (var h = 0; h < nodes.length - 1; h++)
	{
		if (nodes[h].table.length > 0)
		{
			fromListPush(nodes[h], nodes);
		}
	}

	var head = nodes[nodes.length - 1];
	if (head.height > 0 && head.table.length === 1)
	{
		return head.table[0];
	}
	else
	{
		return head;
	}
}

// Push a node into a higher node as a child.
function fromListPush(toPush, nodes)
{
	var h = toPush.height;

	// Maybe the node on this height does not exist.
	if (nodes.length === h)
	{
		var node = {
			ctor: '_Array',
			height: h + 1,
			table: [],
			lengths: []
		};
		nodes.push(node);
	}

	nodes[h].table.push(toPush);
	var len = length(toPush);
	if (nodes[h].lengths.length > 0)
	{
		len += nodes[h].lengths[nodes[h].lengths.length - 1];
	}
	nodes[h].lengths.push(len);

	if (nodes[h].table.length === M)
	{
		fromListPush(nodes[h], nodes);
		nodes[h] = {
			ctor: '_Array',
			height: h + 1,
			table: [],
			lengths: []
		};
	}
}

// Pushes an item via push_ to the bottom right of a tree.
function push(item, a)
{
	var pushed = push_(item, a);
	if (pushed !== null)
	{
		return pushed;
	}

	var newTree = create(item, a.height);
	return siblise(a, newTree);
}

// Recursively tries to push an item to the bottom-right most
// tree possible. If there is no space left for the item,
// null will be returned.
function push_(item, a)
{
	// Handle resursion stop at leaf level.
	if (a.height === 0)
	{
		if (a.table.length < M)
		{
			var newA = {
				ctor: '_Array',
				height: 0,
				table: a.table.slice()
			};
			newA.table.push(item);
			return newA;
		}
		else
		{
		  return null;
		}
	}

	// Recursively push
	var pushed = push_(item, botRight(a));

	// There was space in the bottom right tree, so the slot will
	// be updated.
	if (pushed !== null)
	{
		var newA = nodeCopy(a);
		newA.table[newA.table.length - 1] = pushed;
		newA.lengths[newA.lengths.length - 1]++;
		return newA;
	}

	// When there was no space left, check if there is space left
	// for a new slot with a tree which contains only the item
	// at the bottom.
	if (a.table.length < M)
	{
		var newSlot = create(item, a.height - 1);
		var newA = nodeCopy(a);
		newA.table.push(newSlot);
		newA.lengths.push(newA.lengths[newA.lengths.length - 1] + length(newSlot));
		return newA;
	}
	else
	{
		return null;
	}
}

// Converts an array into a list of elements.
function toList(a)
{
	return toList_(_elm_lang$core$Native_List.Nil, a);
}

function toList_(list, a)
{
	for (var i = a.table.length - 1; i >= 0; i--)
	{
		list =
			a.height === 0
				? _elm_lang$core$Native_List.Cons(a.table[i], list)
				: toList_(list, a.table[i]);
	}
	return list;
}

// Maps a function over the elements of an array.
function map(f, a)
{
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: new Array(a.table.length)
	};
	if (a.height > 0)
	{
		newA.lengths = a.lengths;
	}
	for (var i = 0; i < a.table.length; i++)
	{
		newA.table[i] =
			a.height === 0
				? f(a.table[i])
				: map(f, a.table[i]);
	}
	return newA;
}

// Maps a function over the elements with their index as first argument.
function indexedMap(f, a)
{
	return indexedMap_(f, a, 0);
}

function indexedMap_(f, a, from)
{
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: new Array(a.table.length)
	};
	if (a.height > 0)
	{
		newA.lengths = a.lengths;
	}
	for (var i = 0; i < a.table.length; i++)
	{
		newA.table[i] =
			a.height === 0
				? A2(f, from + i, a.table[i])
				: indexedMap_(f, a.table[i], i == 0 ? from : from + a.lengths[i - 1]);
	}
	return newA;
}

function foldl(f, b, a)
{
	if (a.height === 0)
	{
		for (var i = 0; i < a.table.length; i++)
		{
			b = A2(f, a.table[i], b);
		}
	}
	else
	{
		for (var i = 0; i < a.table.length; i++)
		{
			b = foldl(f, b, a.table[i]);
		}
	}
	return b;
}

function foldr(f, b, a)
{
	if (a.height === 0)
	{
		for (var i = a.table.length; i--; )
		{
			b = A2(f, a.table[i], b);
		}
	}
	else
	{
		for (var i = a.table.length; i--; )
		{
			b = foldr(f, b, a.table[i]);
		}
	}
	return b;
}

// TODO: currently, it slices the right, then the left. This can be
// optimized.
function slice(from, to, a)
{
	if (from < 0)
	{
		from += length(a);
	}
	if (to < 0)
	{
		to += length(a);
	}
	return sliceLeft(from, sliceRight(to, a));
}

function sliceRight(to, a)
{
	if (to === length(a))
	{
		return a;
	}

	// Handle leaf level.
	if (a.height === 0)
	{
		var newA = { ctor:'_Array', height:0 };
		newA.table = a.table.slice(0, to);
		return newA;
	}

	// Slice the right recursively.
	var right = getSlot(to, a);
	var sliced = sliceRight(to - (right > 0 ? a.lengths[right - 1] : 0), a.table[right]);

	// Maybe the a node is not even needed, as sliced contains the whole slice.
	if (right === 0)
	{
		return sliced;
	}

	// Create new node.
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: a.table.slice(0, right),
		lengths: a.lengths.slice(0, right)
	};
	if (sliced.table.length > 0)
	{
		newA.table[right] = sliced;
		newA.lengths[right] = length(sliced) + (right > 0 ? newA.lengths[right - 1] : 0);
	}
	return newA;
}

function sliceLeft(from, a)
{
	if (from === 0)
	{
		return a;
	}

	// Handle leaf level.
	if (a.height === 0)
	{
		var newA = { ctor:'_Array', height:0 };
		newA.table = a.table.slice(from, a.table.length + 1);
		return newA;
	}

	// Slice the left recursively.
	var left = getSlot(from, a);
	var sliced = sliceLeft(from - (left > 0 ? a.lengths[left - 1] : 0), a.table[left]);

	// Maybe the a node is not even needed, as sliced contains the whole slice.
	if (left === a.table.length - 1)
	{
		return sliced;
	}

	// Create new node.
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: a.table.slice(left, a.table.length + 1),
		lengths: new Array(a.table.length - left)
	};
	newA.table[0] = sliced;
	var len = 0;
	for (var i = 0; i < newA.table.length; i++)
	{
		len += length(newA.table[i]);
		newA.lengths[i] = len;
	}

	return newA;
}

// Appends two trees.
function append(a,b)
{
	if (a.table.length === 0)
	{
		return b;
	}
	if (b.table.length === 0)
	{
		return a;
	}

	var c = append_(a, b);

	// Check if both nodes can be crunshed together.
	if (c[0].table.length + c[1].table.length <= M)
	{
		if (c[0].table.length === 0)
		{
			return c[1];
		}
		if (c[1].table.length === 0)
		{
			return c[0];
		}

		// Adjust .table and .lengths
		c[0].table = c[0].table.concat(c[1].table);
		if (c[0].height > 0)
		{
			var len = length(c[0]);
			for (var i = 0; i < c[1].lengths.length; i++)
			{
				c[1].lengths[i] += len;
			}
			c[0].lengths = c[0].lengths.concat(c[1].lengths);
		}

		return c[0];
	}

	if (c[0].height > 0)
	{
		var toRemove = calcToRemove(a, b);
		if (toRemove > E)
		{
			c = shuffle(c[0], c[1], toRemove);
		}
	}

	return siblise(c[0], c[1]);
}

// Returns an array of two nodes; right and left. One node _may_ be empty.
function append_(a, b)
{
	if (a.height === 0 && b.height === 0)
	{
		return [a, b];
	}

	if (a.height !== 1 || b.height !== 1)
	{
		if (a.height === b.height)
		{
			a = nodeCopy(a);
			b = nodeCopy(b);
			var appended = append_(botRight(a), botLeft(b));

			insertRight(a, appended[1]);
			insertLeft(b, appended[0]);
		}
		else if (a.height > b.height)
		{
			a = nodeCopy(a);
			var appended = append_(botRight(a), b);

			insertRight(a, appended[0]);
			b = parentise(appended[1], appended[1].height + 1);
		}
		else
		{
			b = nodeCopy(b);
			var appended = append_(a, botLeft(b));

			var left = appended[0].table.length === 0 ? 0 : 1;
			var right = left === 0 ? 1 : 0;
			insertLeft(b, appended[left]);
			a = parentise(appended[right], appended[right].height + 1);
		}
	}

	// Check if balancing is needed and return based on that.
	if (a.table.length === 0 || b.table.length === 0)
	{
		return [a, b];
	}

	var toRemove = calcToRemove(a, b);
	if (toRemove <= E)
	{
		return [a, b];
	}
	return shuffle(a, b, toRemove);
}

// Helperfunctions for append_. Replaces a child node at the side of the parent.
function insertRight(parent, node)
{
	var index = parent.table.length - 1;
	parent.table[index] = node;
	parent.lengths[index] = length(node);
	parent.lengths[index] += index > 0 ? parent.lengths[index - 1] : 0;
}

function insertLeft(parent, node)
{
	if (node.table.length > 0)
	{
		parent.table[0] = node;
		parent.lengths[0] = length(node);

		var len = length(parent.table[0]);
		for (var i = 1; i < parent.lengths.length; i++)
		{
			len += length(parent.table[i]);
			parent.lengths[i] = len;
		}
	}
	else
	{
		parent.table.shift();
		for (var i = 1; i < parent.lengths.length; i++)
		{
			parent.lengths[i] = parent.lengths[i] - parent.lengths[0];
		}
		parent.lengths.shift();
	}
}

// Returns the extra search steps for E. Refer to the paper.
function calcToRemove(a, b)
{
	var subLengths = 0;
	for (var i = 0; i < a.table.length; i++)
	{
		subLengths += a.table[i].table.length;
	}
	for (var i = 0; i < b.table.length; i++)
	{
		subLengths += b.table[i].table.length;
	}

	var toRemove = a.table.length + b.table.length;
	return toRemove - (Math.floor((subLengths - 1) / M) + 1);
}

// get2, set2 and saveSlot are helpers for accessing elements over two arrays.
function get2(a, b, index)
{
	return index < a.length
		? a[index]
		: b[index - a.length];
}

function set2(a, b, index, value)
{
	if (index < a.length)
	{
		a[index] = value;
	}
	else
	{
		b[index - a.length] = value;
	}
}

function saveSlot(a, b, index, slot)
{
	set2(a.table, b.table, index, slot);

	var l = (index === 0 || index === a.lengths.length)
		? 0
		: get2(a.lengths, a.lengths, index - 1);

	set2(a.lengths, b.lengths, index, l + length(slot));
}

// Creates a node or leaf with a given length at their arrays for perfomance.
// Is only used by shuffle.
function createNode(h, length)
{
	if (length < 0)
	{
		length = 0;
	}
	var a = {
		ctor: '_Array',
		height: h,
		table: new Array(length)
	};
	if (h > 0)
	{
		a.lengths = new Array(length);
	}
	return a;
}

// Returns an array of two balanced nodes.
function shuffle(a, b, toRemove)
{
	var newA = createNode(a.height, Math.min(M, a.table.length + b.table.length - toRemove));
	var newB = createNode(a.height, newA.table.length - (a.table.length + b.table.length - toRemove));

	// Skip the slots with size M. More precise: copy the slot references
	// to the new node
	var read = 0;
	while (get2(a.table, b.table, read).table.length % M === 0)
	{
		set2(newA.table, newB.table, read, get2(a.table, b.table, read));
		set2(newA.lengths, newB.lengths, read, get2(a.lengths, b.lengths, read));
		read++;
	}

	// Pulling items from left to right, caching in a slot before writing
	// it into the new nodes.
	var write = read;
	var slot = new createNode(a.height - 1, 0);
	var from = 0;

	// If the current slot is still containing data, then there will be at
	// least one more write, so we do not break this loop yet.
	while (read - write - (slot.table.length > 0 ? 1 : 0) < toRemove)
	{
		// Find out the max possible items for copying.
		var source = get2(a.table, b.table, read);
		var to = Math.min(M - slot.table.length, source.table.length);

		// Copy and adjust size table.
		slot.table = slot.table.concat(source.table.slice(from, to));
		if (slot.height > 0)
		{
			var len = slot.lengths.length;
			for (var i = len; i < len + to - from; i++)
			{
				slot.lengths[i] = length(slot.table[i]);
				slot.lengths[i] += (i > 0 ? slot.lengths[i - 1] : 0);
			}
		}

		from += to;

		// Only proceed to next slots[i] if the current one was
		// fully copied.
		if (source.table.length <= to)
		{
			read++; from = 0;
		}

		// Only create a new slot if the current one is filled up.
		if (slot.table.length === M)
		{
			saveSlot(newA, newB, write, slot);
			slot = createNode(a.height - 1, 0);
			write++;
		}
	}

	// Cleanup after the loop. Copy the last slot into the new nodes.
	if (slot.table.length > 0)
	{
		saveSlot(newA, newB, write, slot);
		write++;
	}

	// Shift the untouched slots to the left
	while (read < a.table.length + b.table.length )
	{
		saveSlot(newA, newB, write, get2(a.table, b.table, read));
		read++;
		write++;
	}

	return [newA, newB];
}

// Navigation functions
function botRight(a)
{
	return a.table[a.table.length - 1];
}
function botLeft(a)
{
	return a.table[0];
}

// Copies a node for updating. Note that you should not use this if
// only updating only one of "table" or "lengths" for performance reasons.
function nodeCopy(a)
{
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: a.table.slice()
	};
	if (a.height > 0)
	{
		newA.lengths = a.lengths.slice();
	}
	return newA;
}

// Returns how many items are in the tree.
function length(array)
{
	if (array.height === 0)
	{
		return array.table.length;
	}
	else
	{
		return array.lengths[array.lengths.length - 1];
	}
}

// Calculates in which slot of "table" the item probably is, then
// find the exact slot via forward searching in  "lengths". Returns the index.
function getSlot(i, a)
{
	var slot = i >> (5 * a.height);
	while (a.lengths[slot] <= i)
	{
		slot++;
	}
	return slot;
}

// Recursively creates a tree with a given height containing
// only the given item.
function create(item, h)
{
	if (h === 0)
	{
		return {
			ctor: '_Array',
			height: 0,
			table: [item]
		};
	}
	return {
		ctor: '_Array',
		height: h,
		table: [create(item, h - 1)],
		lengths: [1]
	};
}

// Recursively creates a tree that contains the given tree.
function parentise(tree, h)
{
	if (h === tree.height)
	{
		return tree;
	}

	return {
		ctor: '_Array',
		height: h,
		table: [parentise(tree, h - 1)],
		lengths: [length(tree)]
	};
}

// Emphasizes blood brotherhood beneath two trees.
function siblise(a, b)
{
	return {
		ctor: '_Array',
		height: a.height + 1,
		table: [a, b],
		lengths: [length(a), length(a) + length(b)]
	};
}

function toJSArray(a)
{
	var jsArray = new Array(length(a));
	toJSArray_(jsArray, 0, a);
	return jsArray;
}

function toJSArray_(jsArray, i, a)
{
	for (var t = 0; t < a.table.length; t++)
	{
		if (a.height === 0)
		{
			jsArray[i + t] = a.table[t];
		}
		else
		{
			var inc = t === 0 ? 0 : a.lengths[t - 1];
			toJSArray_(jsArray, i + inc, a.table[t]);
		}
	}
}

function fromJSArray(jsArray)
{
	if (jsArray.length === 0)
	{
		return empty;
	}
	var h = Math.floor(Math.log(jsArray.length) / Math.log(M));
	return fromJSArray_(jsArray, h, 0, jsArray.length);
}

function fromJSArray_(jsArray, h, from, to)
{
	if (h === 0)
	{
		return {
			ctor: '_Array',
			height: 0,
			table: jsArray.slice(from, to)
		};
	}

	var step = Math.pow(M, h);
	var table = new Array(Math.ceil((to - from) / step));
	var lengths = new Array(table.length);
	for (var i = 0; i < table.length; i++)
	{
		table[i] = fromJSArray_(jsArray, h - 1, from + (i * step), Math.min(from + ((i + 1) * step), to));
		lengths[i] = length(table[i]) + (i > 0 ? lengths[i - 1] : 0);
	}
	return {
		ctor: '_Array',
		height: h,
		table: table,
		lengths: lengths
	};
}

return {
	empty: empty,
	fromList: fromList,
	toList: toList,
	initialize: F2(initialize),
	append: F2(append),
	push: F2(push),
	slice: F3(slice),
	get: F2(get),
	set: F3(set),
	map: F2(map),
	indexedMap: F2(indexedMap),
	foldl: F3(foldl),
	foldr: F3(foldr),
	length: length,

	toJSArray: toJSArray,
	fromJSArray: fromJSArray
};

}();
//import Native.Utils //

var _elm_lang$core$Native_Basics = function() {

function div(a, b)
{
	return (a / b) | 0;
}
function rem(a, b)
{
	return a % b;
}
function mod(a, b)
{
	if (b === 0)
	{
		throw new Error('Cannot perform mod 0. Division by zero error.');
	}
	var r = a % b;
	var m = a === 0 ? 0 : (b > 0 ? (a >= 0 ? r : r + b) : -mod(-a, -b));

	return m === b ? 0 : m;
}
function logBase(base, n)
{
	return Math.log(n) / Math.log(base);
}
function negate(n)
{
	return -n;
}
function abs(n)
{
	return n < 0 ? -n : n;
}

function min(a, b)
{
	return _elm_lang$core$Native_Utils.cmp(a, b) < 0 ? a : b;
}
function max(a, b)
{
	return _elm_lang$core$Native_Utils.cmp(a, b) > 0 ? a : b;
}
function clamp(lo, hi, n)
{
	return _elm_lang$core$Native_Utils.cmp(n, lo) < 0
		? lo
		: _elm_lang$core$Native_Utils.cmp(n, hi) > 0
			? hi
			: n;
}

var ord = ['LT', 'EQ', 'GT'];

function compare(x, y)
{
	return { ctor: ord[_elm_lang$core$Native_Utils.cmp(x, y) + 1] };
}

function xor(a, b)
{
	return a !== b;
}
function not(b)
{
	return !b;
}
function isInfinite(n)
{
	return n === Infinity || n === -Infinity;
}

function truncate(n)
{
	return n | 0;
}

function degrees(d)
{
	return d * Math.PI / 180;
}
function turns(t)
{
	return 2 * Math.PI * t;
}
function fromPolar(point)
{
	var r = point._0;
	var t = point._1;
	return _elm_lang$core$Native_Utils.Tuple2(r * Math.cos(t), r * Math.sin(t));
}
function toPolar(point)
{
	var x = point._0;
	var y = point._1;
	return _elm_lang$core$Native_Utils.Tuple2(Math.sqrt(x * x + y * y), Math.atan2(y, x));
}

return {
	div: F2(div),
	rem: F2(rem),
	mod: F2(mod),

	pi: Math.PI,
	e: Math.E,
	cos: Math.cos,
	sin: Math.sin,
	tan: Math.tan,
	acos: Math.acos,
	asin: Math.asin,
	atan: Math.atan,
	atan2: F2(Math.atan2),

	degrees: degrees,
	turns: turns,
	fromPolar: fromPolar,
	toPolar: toPolar,

	sqrt: Math.sqrt,
	logBase: F2(logBase),
	negate: negate,
	abs: abs,
	min: F2(min),
	max: F2(max),
	clamp: F3(clamp),
	compare: F2(compare),

	xor: F2(xor),
	not: not,

	truncate: truncate,
	ceiling: Math.ceil,
	floor: Math.floor,
	round: Math.round,
	toFloat: function(x) { return x; },
	isNaN: isNaN,
	isInfinite: isInfinite
};

}();
//import //

var _elm_lang$core$Native_Utils = function() {

// COMPARISONS

function eq(x, y)
{
	var stack = [];
	var isEqual = eqHelp(x, y, 0, stack);
	var pair;
	while (isEqual && (pair = stack.pop()))
	{
		isEqual = eqHelp(pair.x, pair.y, 0, stack);
	}
	return isEqual;
}


function eqHelp(x, y, depth, stack)
{
	if (depth > 100)
	{
		stack.push({ x: x, y: y });
		return true;
	}

	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object')
	{
		if (typeof x === 'function')
		{
			throw new Error(
				'Trying to use `(==)` on functions. There is no way to know if functions are "the same" in the Elm sense.'
				+ ' Read more about this at http://package.elm-lang.org/packages/elm-lang/core/latest/Basics#=='
				+ ' which describes why it is this way and what the better version will look like.'
			);
		}
		return false;
	}

	if (x === null || y === null)
	{
		return false
	}

	if (x instanceof Date)
	{
		return x.getTime() === y.getTime();
	}

	if (!('ctor' in x))
	{
		for (var key in x)
		{
			if (!eqHelp(x[key], y[key], depth + 1, stack))
			{
				return false;
			}
		}
		return true;
	}

	// convert Dicts and Sets to lists
	if (x.ctor === 'RBNode_elm_builtin' || x.ctor === 'RBEmpty_elm_builtin')
	{
		x = _elm_lang$core$Dict$toList(x);
		y = _elm_lang$core$Dict$toList(y);
	}
	if (x.ctor === 'Set_elm_builtin')
	{
		x = _elm_lang$core$Set$toList(x);
		y = _elm_lang$core$Set$toList(y);
	}

	// check if lists are equal without recursion
	if (x.ctor === '::')
	{
		var a = x;
		var b = y;
		while (a.ctor === '::' && b.ctor === '::')
		{
			if (!eqHelp(a._0, b._0, depth + 1, stack))
			{
				return false;
			}
			a = a._1;
			b = b._1;
		}
		return a.ctor === b.ctor;
	}

	// check if Arrays are equal
	if (x.ctor === '_Array')
	{
		var xs = _elm_lang$core$Native_Array.toJSArray(x);
		var ys = _elm_lang$core$Native_Array.toJSArray(y);
		if (xs.length !== ys.length)
		{
			return false;
		}
		for (var i = 0; i < xs.length; i++)
		{
			if (!eqHelp(xs[i], ys[i], depth + 1, stack))
			{
				return false;
			}
		}
		return true;
	}

	if (!eqHelp(x.ctor, y.ctor, depth + 1, stack))
	{
		return false;
	}

	for (var key in x)
	{
		if (!eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

var LT = -1, EQ = 0, GT = 1;

function cmp(x, y)
{
	if (typeof x !== 'object')
	{
		return x === y ? EQ : x < y ? LT : GT;
	}

	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? EQ : a < b ? LT : GT;
	}

	if (x.ctor === '::' || x.ctor === '[]')
	{
		while (x.ctor === '::' && y.ctor === '::')
		{
			var ord = cmp(x._0, y._0);
			if (ord !== EQ)
			{
				return ord;
			}
			x = x._1;
			y = y._1;
		}
		return x.ctor === y.ctor ? EQ : x.ctor === '[]' ? LT : GT;
	}

	if (x.ctor.slice(0, 6) === '_Tuple')
	{
		var ord;
		var n = x.ctor.slice(6) - 0;
		var err = 'cannot compare tuples with more than 6 elements.';
		if (n === 0) return EQ;
		if (n >= 1) { ord = cmp(x._0, y._0); if (ord !== EQ) return ord;
		if (n >= 2) { ord = cmp(x._1, y._1); if (ord !== EQ) return ord;
		if (n >= 3) { ord = cmp(x._2, y._2); if (ord !== EQ) return ord;
		if (n >= 4) { ord = cmp(x._3, y._3); if (ord !== EQ) return ord;
		if (n >= 5) { ord = cmp(x._4, y._4); if (ord !== EQ) return ord;
		if (n >= 6) { ord = cmp(x._5, y._5); if (ord !== EQ) return ord;
		if (n >= 7) throw new Error('Comparison error: ' + err); } } } } } }
		return EQ;
	}

	throw new Error(
		'Comparison error: comparison is only defined on ints, '
		+ 'floats, times, chars, strings, lists of comparable values, '
		+ 'and tuples of comparable values.'
	);
}


// COMMON VALUES

var Tuple0 = {
	ctor: '_Tuple0'
};

function Tuple2(x, y)
{
	return {
		ctor: '_Tuple2',
		_0: x,
		_1: y
	};
}

function chr(c)
{
	return new String(c);
}


// GUID

var count = 0;
function guid(_)
{
	return count++;
}


// RECORDS

function update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


//// LIST STUFF ////

var Nil = { ctor: '[]' };

function Cons(hd, tl)
{
	return {
		ctor: '::',
		_0: hd,
		_1: tl
	};
}

function append(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (xs.ctor === '[]')
	{
		return ys;
	}
	var root = Cons(xs._0, Nil);
	var curr = root;
	xs = xs._1;
	while (xs.ctor !== '[]')
	{
		curr._1 = Cons(xs._0, Nil);
		xs = xs._1;
		curr = curr._1;
	}
	curr._1 = ys;
	return root;
}


// CRASHES

function crash(moduleName, region)
{
	return function(message) {
		throw new Error(
			'Ran into a `Debug.crash` in module `' + moduleName + '` ' + regionToString(region) + '\n'
			+ 'The message provided by the code author is:\n\n    '
			+ message
		);
	};
}

function crashCase(moduleName, region, value)
{
	return function(message) {
		throw new Error(
			'Ran into a `Debug.crash` in module `' + moduleName + '`\n\n'
			+ 'This was caused by the `case` expression ' + regionToString(region) + '.\n'
			+ 'One of the branches ended with a crash and the following value got through:\n\n    ' + toString(value) + '\n\n'
			+ 'The message provided by the code author is:\n\n    '
			+ message
		);
	};
}

function regionToString(region)
{
	if (region.start.line == region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'between lines ' + region.start.line + ' and ' + region.end.line;
}


// TO STRING

function toString(v)
{
	var type = typeof v;
	if (type === 'function')
	{
		return '<function>';
	}

	if (type === 'boolean')
	{
		return v ? 'True' : 'False';
	}

	if (type === 'number')
	{
		return v + '';
	}

	if (v instanceof String)
	{
		return '\'' + addSlashes(v, true) + '\'';
	}

	if (type === 'string')
	{
		return '"' + addSlashes(v, false) + '"';
	}

	if (v === null)
	{
		return 'null';
	}

	if (type === 'object' && 'ctor' in v)
	{
		var ctorStarter = v.ctor.substring(0, 5);

		if (ctorStarter === '_Tupl')
		{
			var output = [];
			for (var k in v)
			{
				if (k === 'ctor') continue;
				output.push(toString(v[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (ctorStarter === '_Task')
		{
			return '<task>'
		}

		if (v.ctor === '_Array')
		{
			var list = _elm_lang$core$Array$toList(v);
			return 'Array.fromList ' + toString(list);
		}

		if (v.ctor === '<decoder>')
		{
			return '<decoder>';
		}

		if (v.ctor === '_Process')
		{
			return '<process:' + v.id + '>';
		}

		if (v.ctor === '::')
		{
			var output = '[' + toString(v._0);
			v = v._1;
			while (v.ctor === '::')
			{
				output += ',' + toString(v._0);
				v = v._1;
			}
			return output + ']';
		}

		if (v.ctor === '[]')
		{
			return '[]';
		}

		if (v.ctor === 'Set_elm_builtin')
		{
			return 'Set.fromList ' + toString(_elm_lang$core$Set$toList(v));
		}

		if (v.ctor === 'RBNode_elm_builtin' || v.ctor === 'RBEmpty_elm_builtin')
		{
			return 'Dict.fromList ' + toString(_elm_lang$core$Dict$toList(v));
		}

		var output = '';
		for (var i in v)
		{
			if (i === 'ctor') continue;
			var str = toString(v[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return v.ctor + output;
	}

	if (type === 'object')
	{
		if (v instanceof Date)
		{
			return '<' + v.toString() + '>';
		}

		if (v.elm_web_socket)
		{
			return '<websocket>';
		}

		var output = [];
		for (var k in v)
		{
			output.push(k + ' = ' + toString(v[k]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return '<internal structure>';
}

function addSlashes(str, isChar)
{
	var s = str.replace(/\\/g, '\\\\')
			  .replace(/\n/g, '\\n')
			  .replace(/\t/g, '\\t')
			  .replace(/\r/g, '\\r')
			  .replace(/\v/g, '\\v')
			  .replace(/\0/g, '\\0');
	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}


return {
	eq: eq,
	cmp: cmp,
	Tuple0: Tuple0,
	Tuple2: Tuple2,
	chr: chr,
	update: update,
	guid: guid,

	append: F2(append),

	crash: crash,
	crashCase: crashCase,

	toString: toString
};

}();
var _elm_lang$core$Basics$never = function (_p0) {
	never:
	while (true) {
		var _p1 = _p0;
		var _v1 = _p1._0;
		_p0 = _v1;
		continue never;
	}
};
var _elm_lang$core$Basics$uncurry = F2(
	function (f, _p2) {
		var _p3 = _p2;
		return A2(f, _p3._0, _p3._1);
	});
var _elm_lang$core$Basics$curry = F3(
	function (f, a, b) {
		return f(
			{ctor: '_Tuple2', _0: a, _1: b});
	});
var _elm_lang$core$Basics$flip = F3(
	function (f, b, a) {
		return A2(f, a, b);
	});
var _elm_lang$core$Basics$always = F2(
	function (a, _p4) {
		return a;
	});
var _elm_lang$core$Basics$identity = function (x) {
	return x;
};
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<|'] = F2(
	function (f, x) {
		return f(x);
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['|>'] = F2(
	function (x, f) {
		return f(x);
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['>>'] = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<<'] = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['++'] = _elm_lang$core$Native_Utils.append;
var _elm_lang$core$Basics$toString = _elm_lang$core$Native_Utils.toString;
var _elm_lang$core$Basics$isInfinite = _elm_lang$core$Native_Basics.isInfinite;
var _elm_lang$core$Basics$isNaN = _elm_lang$core$Native_Basics.isNaN;
var _elm_lang$core$Basics$toFloat = _elm_lang$core$Native_Basics.toFloat;
var _elm_lang$core$Basics$ceiling = _elm_lang$core$Native_Basics.ceiling;
var _elm_lang$core$Basics$floor = _elm_lang$core$Native_Basics.floor;
var _elm_lang$core$Basics$truncate = _elm_lang$core$Native_Basics.truncate;
var _elm_lang$core$Basics$round = _elm_lang$core$Native_Basics.round;
var _elm_lang$core$Basics$not = _elm_lang$core$Native_Basics.not;
var _elm_lang$core$Basics$xor = _elm_lang$core$Native_Basics.xor;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['||'] = _elm_lang$core$Native_Basics.or;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['&&'] = _elm_lang$core$Native_Basics.and;
var _elm_lang$core$Basics$max = _elm_lang$core$Native_Basics.max;
var _elm_lang$core$Basics$min = _elm_lang$core$Native_Basics.min;
var _elm_lang$core$Basics$compare = _elm_lang$core$Native_Basics.compare;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['>='] = _elm_lang$core$Native_Basics.ge;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<='] = _elm_lang$core$Native_Basics.le;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['>'] = _elm_lang$core$Native_Basics.gt;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<'] = _elm_lang$core$Native_Basics.lt;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['/='] = _elm_lang$core$Native_Basics.neq;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['=='] = _elm_lang$core$Native_Basics.eq;
var _elm_lang$core$Basics$e = _elm_lang$core$Native_Basics.e;
var _elm_lang$core$Basics$pi = _elm_lang$core$Native_Basics.pi;
var _elm_lang$core$Basics$clamp = _elm_lang$core$Native_Basics.clamp;
var _elm_lang$core$Basics$logBase = _elm_lang$core$Native_Basics.logBase;
var _elm_lang$core$Basics$abs = _elm_lang$core$Native_Basics.abs;
var _elm_lang$core$Basics$negate = _elm_lang$core$Native_Basics.negate;
var _elm_lang$core$Basics$sqrt = _elm_lang$core$Native_Basics.sqrt;
var _elm_lang$core$Basics$atan2 = _elm_lang$core$Native_Basics.atan2;
var _elm_lang$core$Basics$atan = _elm_lang$core$Native_Basics.atan;
var _elm_lang$core$Basics$asin = _elm_lang$core$Native_Basics.asin;
var _elm_lang$core$Basics$acos = _elm_lang$core$Native_Basics.acos;
var _elm_lang$core$Basics$tan = _elm_lang$core$Native_Basics.tan;
var _elm_lang$core$Basics$sin = _elm_lang$core$Native_Basics.sin;
var _elm_lang$core$Basics$cos = _elm_lang$core$Native_Basics.cos;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['^'] = _elm_lang$core$Native_Basics.exp;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['%'] = _elm_lang$core$Native_Basics.mod;
var _elm_lang$core$Basics$rem = _elm_lang$core$Native_Basics.rem;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['//'] = _elm_lang$core$Native_Basics.div;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['/'] = _elm_lang$core$Native_Basics.floatDiv;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['*'] = _elm_lang$core$Native_Basics.mul;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['-'] = _elm_lang$core$Native_Basics.sub;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['+'] = _elm_lang$core$Native_Basics.add;
var _elm_lang$core$Basics$toPolar = _elm_lang$core$Native_Basics.toPolar;
var _elm_lang$core$Basics$fromPolar = _elm_lang$core$Native_Basics.fromPolar;
var _elm_lang$core$Basics$turns = _elm_lang$core$Native_Basics.turns;
var _elm_lang$core$Basics$degrees = _elm_lang$core$Native_Basics.degrees;
var _elm_lang$core$Basics$radians = function (t) {
	return t;
};
var _elm_lang$core$Basics$GT = {ctor: 'GT'};
var _elm_lang$core$Basics$EQ = {ctor: 'EQ'};
var _elm_lang$core$Basics$LT = {ctor: 'LT'};
var _elm_lang$core$Basics$JustOneMore = function (a) {
	return {ctor: 'JustOneMore', _0: a};
};

var _elm_lang$core$Maybe$withDefault = F2(
	function ($default, maybe) {
		var _p0 = maybe;
		if (_p0.ctor === 'Just') {
			return _p0._0;
		} else {
			return $default;
		}
	});
var _elm_lang$core$Maybe$Nothing = {ctor: 'Nothing'};
var _elm_lang$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		var _p1 = maybeValue;
		if (_p1.ctor === 'Just') {
			return callback(_p1._0);
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$Just = function (a) {
	return {ctor: 'Just', _0: a};
};
var _elm_lang$core$Maybe$map = F2(
	function (f, maybe) {
		var _p2 = maybe;
		if (_p2.ctor === 'Just') {
			return _elm_lang$core$Maybe$Just(
				f(_p2._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map2 = F3(
	function (func, ma, mb) {
		var _p3 = {ctor: '_Tuple2', _0: ma, _1: mb};
		if (((_p3.ctor === '_Tuple2') && (_p3._0.ctor === 'Just')) && (_p3._1.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A2(func, _p3._0._0, _p3._1._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map3 = F4(
	function (func, ma, mb, mc) {
		var _p4 = {ctor: '_Tuple3', _0: ma, _1: mb, _2: mc};
		if ((((_p4.ctor === '_Tuple3') && (_p4._0.ctor === 'Just')) && (_p4._1.ctor === 'Just')) && (_p4._2.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A3(func, _p4._0._0, _p4._1._0, _p4._2._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map4 = F5(
	function (func, ma, mb, mc, md) {
		var _p5 = {ctor: '_Tuple4', _0: ma, _1: mb, _2: mc, _3: md};
		if (((((_p5.ctor === '_Tuple4') && (_p5._0.ctor === 'Just')) && (_p5._1.ctor === 'Just')) && (_p5._2.ctor === 'Just')) && (_p5._3.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A4(func, _p5._0._0, _p5._1._0, _p5._2._0, _p5._3._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map5 = F6(
	function (func, ma, mb, mc, md, me) {
		var _p6 = {ctor: '_Tuple5', _0: ma, _1: mb, _2: mc, _3: md, _4: me};
		if ((((((_p6.ctor === '_Tuple5') && (_p6._0.ctor === 'Just')) && (_p6._1.ctor === 'Just')) && (_p6._2.ctor === 'Just')) && (_p6._3.ctor === 'Just')) && (_p6._4.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A5(func, _p6._0._0, _p6._1._0, _p6._2._0, _p6._3._0, _p6._4._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});

//import Native.Utils //

var _elm_lang$core$Native_List = function() {

var Nil = { ctor: '[]' };

function Cons(hd, tl)
{
	return { ctor: '::', _0: hd, _1: tl };
}

function fromArray(arr)
{
	var out = Nil;
	for (var i = arr.length; i--; )
	{
		out = Cons(arr[i], out);
	}
	return out;
}

function toArray(xs)
{
	var out = [];
	while (xs.ctor !== '[]')
	{
		out.push(xs._0);
		xs = xs._1;
	}
	return out;
}

function foldr(f, b, xs)
{
	var arr = toArray(xs);
	var acc = b;
	for (var i = arr.length; i--; )
	{
		acc = A2(f, arr[i], acc);
	}
	return acc;
}

function map2(f, xs, ys)
{
	var arr = [];
	while (xs.ctor !== '[]' && ys.ctor !== '[]')
	{
		arr.push(A2(f, xs._0, ys._0));
		xs = xs._1;
		ys = ys._1;
	}
	return fromArray(arr);
}

function map3(f, xs, ys, zs)
{
	var arr = [];
	while (xs.ctor !== '[]' && ys.ctor !== '[]' && zs.ctor !== '[]')
	{
		arr.push(A3(f, xs._0, ys._0, zs._0));
		xs = xs._1;
		ys = ys._1;
		zs = zs._1;
	}
	return fromArray(arr);
}

function map4(f, ws, xs, ys, zs)
{
	var arr = [];
	while (   ws.ctor !== '[]'
		   && xs.ctor !== '[]'
		   && ys.ctor !== '[]'
		   && zs.ctor !== '[]')
	{
		arr.push(A4(f, ws._0, xs._0, ys._0, zs._0));
		ws = ws._1;
		xs = xs._1;
		ys = ys._1;
		zs = zs._1;
	}
	return fromArray(arr);
}

function map5(f, vs, ws, xs, ys, zs)
{
	var arr = [];
	while (   vs.ctor !== '[]'
		   && ws.ctor !== '[]'
		   && xs.ctor !== '[]'
		   && ys.ctor !== '[]'
		   && zs.ctor !== '[]')
	{
		arr.push(A5(f, vs._0, ws._0, xs._0, ys._0, zs._0));
		vs = vs._1;
		ws = ws._1;
		xs = xs._1;
		ys = ys._1;
		zs = zs._1;
	}
	return fromArray(arr);
}

function sortBy(f, xs)
{
	return fromArray(toArray(xs).sort(function(a, b) {
		return _elm_lang$core$Native_Utils.cmp(f(a), f(b));
	}));
}

function sortWith(f, xs)
{
	return fromArray(toArray(xs).sort(function(a, b) {
		var ord = f(a)(b).ctor;
		return ord === 'EQ' ? 0 : ord === 'LT' ? -1 : 1;
	}));
}

return {
	Nil: Nil,
	Cons: Cons,
	cons: F2(Cons),
	toArray: toArray,
	fromArray: fromArray,

	foldr: F3(foldr),

	map2: F3(map2),
	map3: F4(map3),
	map4: F5(map4),
	map5: F6(map5),
	sortBy: F2(sortBy),
	sortWith: F2(sortWith)
};

}();
var _elm_lang$core$List$sortWith = _elm_lang$core$Native_List.sortWith;
var _elm_lang$core$List$sortBy = _elm_lang$core$Native_List.sortBy;
var _elm_lang$core$List$sort = function (xs) {
	return A2(_elm_lang$core$List$sortBy, _elm_lang$core$Basics$identity, xs);
};
var _elm_lang$core$List$singleton = function (value) {
	return {
		ctor: '::',
		_0: value,
		_1: {ctor: '[]'}
	};
};
var _elm_lang$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
				return list;
			} else {
				var _p0 = list;
				if (_p0.ctor === '[]') {
					return list;
				} else {
					var _v1 = n - 1,
						_v2 = _p0._1;
					n = _v1;
					list = _v2;
					continue drop;
				}
			}
		}
	});
var _elm_lang$core$List$map5 = _elm_lang$core$Native_List.map5;
var _elm_lang$core$List$map4 = _elm_lang$core$Native_List.map4;
var _elm_lang$core$List$map3 = _elm_lang$core$Native_List.map3;
var _elm_lang$core$List$map2 = _elm_lang$core$Native_List.map2;
var _elm_lang$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			var _p1 = list;
			if (_p1.ctor === '[]') {
				return false;
			} else {
				if (isOkay(_p1._0)) {
					return true;
				} else {
					var _v4 = isOkay,
						_v5 = _p1._1;
					isOkay = _v4;
					list = _v5;
					continue any;
				}
			}
		}
	});
var _elm_lang$core$List$all = F2(
	function (isOkay, list) {
		return !A2(
			_elm_lang$core$List$any,
			function (_p2) {
				return !isOkay(_p2);
			},
			list);
	});
var _elm_lang$core$List$foldr = _elm_lang$core$Native_List.foldr;
var _elm_lang$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			var _p3 = list;
			if (_p3.ctor === '[]') {
				return acc;
			} else {
				var _v7 = func,
					_v8 = A2(func, _p3._0, acc),
					_v9 = _p3._1;
				func = _v7;
				acc = _v8;
				list = _v9;
				continue foldl;
			}
		}
	});
var _elm_lang$core$List$length = function (xs) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (_p4, i) {
				return i + 1;
			}),
		0,
		xs);
};
var _elm_lang$core$List$sum = function (numbers) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (x, y) {
				return x + y;
			}),
		0,
		numbers);
};
var _elm_lang$core$List$product = function (numbers) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (x, y) {
				return x * y;
			}),
		1,
		numbers);
};
var _elm_lang$core$List$maximum = function (list) {
	var _p5 = list;
	if (_p5.ctor === '::') {
		return _elm_lang$core$Maybe$Just(
			A3(_elm_lang$core$List$foldl, _elm_lang$core$Basics$max, _p5._0, _p5._1));
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List$minimum = function (list) {
	var _p6 = list;
	if (_p6.ctor === '::') {
		return _elm_lang$core$Maybe$Just(
			A3(_elm_lang$core$List$foldl, _elm_lang$core$Basics$min, _p6._0, _p6._1));
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List$member = F2(
	function (x, xs) {
		return A2(
			_elm_lang$core$List$any,
			function (a) {
				return _elm_lang$core$Native_Utils.eq(a, x);
			},
			xs);
	});
var _elm_lang$core$List$isEmpty = function (xs) {
	var _p7 = xs;
	if (_p7.ctor === '[]') {
		return true;
	} else {
		return false;
	}
};
var _elm_lang$core$List$tail = function (list) {
	var _p8 = list;
	if (_p8.ctor === '::') {
		return _elm_lang$core$Maybe$Just(_p8._1);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List$head = function (list) {
	var _p9 = list;
	if (_p9.ctor === '::') {
		return _elm_lang$core$Maybe$Just(_p9._0);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List_ops = _elm_lang$core$List_ops || {};
_elm_lang$core$List_ops['::'] = _elm_lang$core$Native_List.cons;
var _elm_lang$core$List$map = F2(
	function (f, xs) {
		return A3(
			_elm_lang$core$List$foldr,
			F2(
				function (x, acc) {
					return {
						ctor: '::',
						_0: f(x),
						_1: acc
					};
				}),
			{ctor: '[]'},
			xs);
	});
var _elm_lang$core$List$filter = F2(
	function (pred, xs) {
		var conditionalCons = F2(
			function (front, back) {
				return pred(front) ? {ctor: '::', _0: front, _1: back} : back;
			});
		return A3(
			_elm_lang$core$List$foldr,
			conditionalCons,
			{ctor: '[]'},
			xs);
	});
var _elm_lang$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _p10 = f(mx);
		if (_p10.ctor === 'Just') {
			return {ctor: '::', _0: _p10._0, _1: xs};
		} else {
			return xs;
		}
	});
var _elm_lang$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			_elm_lang$core$List$foldr,
			_elm_lang$core$List$maybeCons(f),
			{ctor: '[]'},
			xs);
	});
var _elm_lang$core$List$reverse = function (list) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (x, y) {
				return {ctor: '::', _0: x, _1: y};
			}),
		{ctor: '[]'},
		list);
};
var _elm_lang$core$List$scanl = F3(
	function (f, b, xs) {
		var scan1 = F2(
			function (x, accAcc) {
				var _p11 = accAcc;
				if (_p11.ctor === '::') {
					return {
						ctor: '::',
						_0: A2(f, x, _p11._0),
						_1: accAcc
					};
				} else {
					return {ctor: '[]'};
				}
			});
		return _elm_lang$core$List$reverse(
			A3(
				_elm_lang$core$List$foldl,
				scan1,
				{
					ctor: '::',
					_0: b,
					_1: {ctor: '[]'}
				},
				xs));
	});
var _elm_lang$core$List$append = F2(
	function (xs, ys) {
		var _p12 = ys;
		if (_p12.ctor === '[]') {
			return xs;
		} else {
			return A3(
				_elm_lang$core$List$foldr,
				F2(
					function (x, y) {
						return {ctor: '::', _0: x, _1: y};
					}),
				ys,
				xs);
		}
	});
var _elm_lang$core$List$concat = function (lists) {
	return A3(
		_elm_lang$core$List$foldr,
		_elm_lang$core$List$append,
		{ctor: '[]'},
		lists);
};
var _elm_lang$core$List$concatMap = F2(
	function (f, list) {
		return _elm_lang$core$List$concat(
			A2(_elm_lang$core$List$map, f, list));
	});
var _elm_lang$core$List$partition = F2(
	function (pred, list) {
		var step = F2(
			function (x, _p13) {
				var _p14 = _p13;
				var _p16 = _p14._0;
				var _p15 = _p14._1;
				return pred(x) ? {
					ctor: '_Tuple2',
					_0: {ctor: '::', _0: x, _1: _p16},
					_1: _p15
				} : {
					ctor: '_Tuple2',
					_0: _p16,
					_1: {ctor: '::', _0: x, _1: _p15}
				};
			});
		return A3(
			_elm_lang$core$List$foldr,
			step,
			{
				ctor: '_Tuple2',
				_0: {ctor: '[]'},
				_1: {ctor: '[]'}
			},
			list);
	});
var _elm_lang$core$List$unzip = function (pairs) {
	var step = F2(
		function (_p18, _p17) {
			var _p19 = _p18;
			var _p20 = _p17;
			return {
				ctor: '_Tuple2',
				_0: {ctor: '::', _0: _p19._0, _1: _p20._0},
				_1: {ctor: '::', _0: _p19._1, _1: _p20._1}
			};
		});
	return A3(
		_elm_lang$core$List$foldr,
		step,
		{
			ctor: '_Tuple2',
			_0: {ctor: '[]'},
			_1: {ctor: '[]'}
		},
		pairs);
};
var _elm_lang$core$List$intersperse = F2(
	function (sep, xs) {
		var _p21 = xs;
		if (_p21.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			var step = F2(
				function (x, rest) {
					return {
						ctor: '::',
						_0: sep,
						_1: {ctor: '::', _0: x, _1: rest}
					};
				});
			var spersed = A3(
				_elm_lang$core$List$foldr,
				step,
				{ctor: '[]'},
				_p21._1);
			return {ctor: '::', _0: _p21._0, _1: spersed};
		}
	});
var _elm_lang$core$List$takeReverse = F3(
	function (n, list, taken) {
		takeReverse:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
				return taken;
			} else {
				var _p22 = list;
				if (_p22.ctor === '[]') {
					return taken;
				} else {
					var _v23 = n - 1,
						_v24 = _p22._1,
						_v25 = {ctor: '::', _0: _p22._0, _1: taken};
					n = _v23;
					list = _v24;
					taken = _v25;
					continue takeReverse;
				}
			}
		}
	});
var _elm_lang$core$List$takeTailRec = F2(
	function (n, list) {
		return _elm_lang$core$List$reverse(
			A3(
				_elm_lang$core$List$takeReverse,
				n,
				list,
				{ctor: '[]'}));
	});
var _elm_lang$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
			return {ctor: '[]'};
		} else {
			var _p23 = {ctor: '_Tuple2', _0: n, _1: list};
			_v26_5:
			do {
				_v26_1:
				do {
					if (_p23.ctor === '_Tuple2') {
						if (_p23._1.ctor === '[]') {
							return list;
						} else {
							if (_p23._1._1.ctor === '::') {
								switch (_p23._0) {
									case 1:
										break _v26_1;
									case 2:
										return {
											ctor: '::',
											_0: _p23._1._0,
											_1: {
												ctor: '::',
												_0: _p23._1._1._0,
												_1: {ctor: '[]'}
											}
										};
									case 3:
										if (_p23._1._1._1.ctor === '::') {
											return {
												ctor: '::',
												_0: _p23._1._0,
												_1: {
													ctor: '::',
													_0: _p23._1._1._0,
													_1: {
														ctor: '::',
														_0: _p23._1._1._1._0,
														_1: {ctor: '[]'}
													}
												}
											};
										} else {
											break _v26_5;
										}
									default:
										if ((_p23._1._1._1.ctor === '::') && (_p23._1._1._1._1.ctor === '::')) {
											var _p28 = _p23._1._1._1._0;
											var _p27 = _p23._1._1._0;
											var _p26 = _p23._1._0;
											var _p25 = _p23._1._1._1._1._0;
											var _p24 = _p23._1._1._1._1._1;
											return (_elm_lang$core$Native_Utils.cmp(ctr, 1000) > 0) ? {
												ctor: '::',
												_0: _p26,
												_1: {
													ctor: '::',
													_0: _p27,
													_1: {
														ctor: '::',
														_0: _p28,
														_1: {
															ctor: '::',
															_0: _p25,
															_1: A2(_elm_lang$core$List$takeTailRec, n - 4, _p24)
														}
													}
												}
											} : {
												ctor: '::',
												_0: _p26,
												_1: {
													ctor: '::',
													_0: _p27,
													_1: {
														ctor: '::',
														_0: _p28,
														_1: {
															ctor: '::',
															_0: _p25,
															_1: A3(_elm_lang$core$List$takeFast, ctr + 1, n - 4, _p24)
														}
													}
												}
											};
										} else {
											break _v26_5;
										}
								}
							} else {
								if (_p23._0 === 1) {
									break _v26_1;
								} else {
									break _v26_5;
								}
							}
						}
					} else {
						break _v26_5;
					}
				} while(false);
				return {
					ctor: '::',
					_0: _p23._1._0,
					_1: {ctor: '[]'}
				};
			} while(false);
			return list;
		}
	});
var _elm_lang$core$List$take = F2(
	function (n, list) {
		return A3(_elm_lang$core$List$takeFast, 0, n, list);
	});
var _elm_lang$core$List$repeatHelp = F3(
	function (result, n, value) {
		repeatHelp:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
				return result;
			} else {
				var _v27 = {ctor: '::', _0: value, _1: result},
					_v28 = n - 1,
					_v29 = value;
				result = _v27;
				n = _v28;
				value = _v29;
				continue repeatHelp;
			}
		}
	});
var _elm_lang$core$List$repeat = F2(
	function (n, value) {
		return A3(
			_elm_lang$core$List$repeatHelp,
			{ctor: '[]'},
			n,
			value);
	});
var _elm_lang$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(lo, hi) < 1) {
				var _v30 = lo,
					_v31 = hi - 1,
					_v32 = {ctor: '::', _0: hi, _1: list};
				lo = _v30;
				hi = _v31;
				list = _v32;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var _elm_lang$core$List$range = F2(
	function (lo, hi) {
		return A3(
			_elm_lang$core$List$rangeHelp,
			lo,
			hi,
			{ctor: '[]'});
	});
var _elm_lang$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			_elm_lang$core$List$map2,
			f,
			A2(
				_elm_lang$core$List$range,
				0,
				_elm_lang$core$List$length(xs) - 1),
			xs);
	});

var _elm_lang$core$Array$append = _elm_lang$core$Native_Array.append;
var _elm_lang$core$Array$length = _elm_lang$core$Native_Array.length;
var _elm_lang$core$Array$isEmpty = function (array) {
	return _elm_lang$core$Native_Utils.eq(
		_elm_lang$core$Array$length(array),
		0);
};
var _elm_lang$core$Array$slice = _elm_lang$core$Native_Array.slice;
var _elm_lang$core$Array$set = _elm_lang$core$Native_Array.set;
var _elm_lang$core$Array$get = F2(
	function (i, array) {
		return ((_elm_lang$core$Native_Utils.cmp(0, i) < 1) && (_elm_lang$core$Native_Utils.cmp(
			i,
			_elm_lang$core$Native_Array.length(array)) < 0)) ? _elm_lang$core$Maybe$Just(
			A2(_elm_lang$core$Native_Array.get, i, array)) : _elm_lang$core$Maybe$Nothing;
	});
var _elm_lang$core$Array$push = _elm_lang$core$Native_Array.push;
var _elm_lang$core$Array$empty = _elm_lang$core$Native_Array.empty;
var _elm_lang$core$Array$filter = F2(
	function (isOkay, arr) {
		var update = F2(
			function (x, xs) {
				return isOkay(x) ? A2(_elm_lang$core$Native_Array.push, x, xs) : xs;
			});
		return A3(_elm_lang$core$Native_Array.foldl, update, _elm_lang$core$Native_Array.empty, arr);
	});
var _elm_lang$core$Array$foldr = _elm_lang$core$Native_Array.foldr;
var _elm_lang$core$Array$foldl = _elm_lang$core$Native_Array.foldl;
var _elm_lang$core$Array$indexedMap = _elm_lang$core$Native_Array.indexedMap;
var _elm_lang$core$Array$map = _elm_lang$core$Native_Array.map;
var _elm_lang$core$Array$toIndexedList = function (array) {
	return A3(
		_elm_lang$core$List$map2,
		F2(
			function (v0, v1) {
				return {ctor: '_Tuple2', _0: v0, _1: v1};
			}),
		A2(
			_elm_lang$core$List$range,
			0,
			_elm_lang$core$Native_Array.length(array) - 1),
		_elm_lang$core$Native_Array.toList(array));
};
var _elm_lang$core$Array$toList = _elm_lang$core$Native_Array.toList;
var _elm_lang$core$Array$fromList = _elm_lang$core$Native_Array.fromList;
var _elm_lang$core$Array$initialize = _elm_lang$core$Native_Array.initialize;
var _elm_lang$core$Array$repeat = F2(
	function (n, e) {
		return A2(
			_elm_lang$core$Array$initialize,
			n,
			_elm_lang$core$Basics$always(e));
	});
var _elm_lang$core$Array$Array = {ctor: 'Array'};

//import Native.Utils //

var _elm_lang$core$Native_Char = function() {

return {
	fromCode: function(c) { return _elm_lang$core$Native_Utils.chr(String.fromCharCode(c)); },
	toCode: function(c) { return c.charCodeAt(0); },
	toUpper: function(c) { return _elm_lang$core$Native_Utils.chr(c.toUpperCase()); },
	toLower: function(c) { return _elm_lang$core$Native_Utils.chr(c.toLowerCase()); },
	toLocaleUpper: function(c) { return _elm_lang$core$Native_Utils.chr(c.toLocaleUpperCase()); },
	toLocaleLower: function(c) { return _elm_lang$core$Native_Utils.chr(c.toLocaleLowerCase()); }
};

}();
var _elm_lang$core$Char$fromCode = _elm_lang$core$Native_Char.fromCode;
var _elm_lang$core$Char$toCode = _elm_lang$core$Native_Char.toCode;
var _elm_lang$core$Char$toLocaleLower = _elm_lang$core$Native_Char.toLocaleLower;
var _elm_lang$core$Char$toLocaleUpper = _elm_lang$core$Native_Char.toLocaleUpper;
var _elm_lang$core$Char$toLower = _elm_lang$core$Native_Char.toLower;
var _elm_lang$core$Char$toUpper = _elm_lang$core$Native_Char.toUpper;
var _elm_lang$core$Char$isBetween = F3(
	function (low, high, $char) {
		var code = _elm_lang$core$Char$toCode($char);
		return (_elm_lang$core$Native_Utils.cmp(
			code,
			_elm_lang$core$Char$toCode(low)) > -1) && (_elm_lang$core$Native_Utils.cmp(
			code,
			_elm_lang$core$Char$toCode(high)) < 1);
	});
var _elm_lang$core$Char$isUpper = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('A'),
	_elm_lang$core$Native_Utils.chr('Z'));
var _elm_lang$core$Char$isLower = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('a'),
	_elm_lang$core$Native_Utils.chr('z'));
var _elm_lang$core$Char$isDigit = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('0'),
	_elm_lang$core$Native_Utils.chr('9'));
var _elm_lang$core$Char$isOctDigit = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('0'),
	_elm_lang$core$Native_Utils.chr('7'));
var _elm_lang$core$Char$isHexDigit = function ($char) {
	return _elm_lang$core$Char$isDigit($char) || (A3(
		_elm_lang$core$Char$isBetween,
		_elm_lang$core$Native_Utils.chr('a'),
		_elm_lang$core$Native_Utils.chr('f'),
		$char) || A3(
		_elm_lang$core$Char$isBetween,
		_elm_lang$core$Native_Utils.chr('A'),
		_elm_lang$core$Native_Utils.chr('F'),
		$char));
};

var _elm_lang$core$Color$fmod = F2(
	function (f, n) {
		var integer = _elm_lang$core$Basics$floor(f);
		return (_elm_lang$core$Basics$toFloat(
			A2(_elm_lang$core$Basics_ops['%'], integer, n)) + f) - _elm_lang$core$Basics$toFloat(integer);
	});
var _elm_lang$core$Color$rgbToHsl = F3(
	function (red, green, blue) {
		var b = _elm_lang$core$Basics$toFloat(blue) / 255;
		var g = _elm_lang$core$Basics$toFloat(green) / 255;
		var r = _elm_lang$core$Basics$toFloat(red) / 255;
		var cMax = A2(
			_elm_lang$core$Basics$max,
			A2(_elm_lang$core$Basics$max, r, g),
			b);
		var cMin = A2(
			_elm_lang$core$Basics$min,
			A2(_elm_lang$core$Basics$min, r, g),
			b);
		var c = cMax - cMin;
		var lightness = (cMax + cMin) / 2;
		var saturation = _elm_lang$core$Native_Utils.eq(lightness, 0) ? 0 : (c / (1 - _elm_lang$core$Basics$abs((2 * lightness) - 1)));
		var hue = _elm_lang$core$Basics$degrees(60) * (_elm_lang$core$Native_Utils.eq(cMax, r) ? A2(_elm_lang$core$Color$fmod, (g - b) / c, 6) : (_elm_lang$core$Native_Utils.eq(cMax, g) ? (((b - r) / c) + 2) : (((r - g) / c) + 4)));
		return {ctor: '_Tuple3', _0: hue, _1: saturation, _2: lightness};
	});
var _elm_lang$core$Color$hslToRgb = F3(
	function (hue, saturation, lightness) {
		var normHue = hue / _elm_lang$core$Basics$degrees(60);
		var chroma = (1 - _elm_lang$core$Basics$abs((2 * lightness) - 1)) * saturation;
		var x = chroma * (1 - _elm_lang$core$Basics$abs(
			A2(_elm_lang$core$Color$fmod, normHue, 2) - 1));
		var _p0 = (_elm_lang$core$Native_Utils.cmp(normHue, 0) < 0) ? {ctor: '_Tuple3', _0: 0, _1: 0, _2: 0} : ((_elm_lang$core$Native_Utils.cmp(normHue, 1) < 0) ? {ctor: '_Tuple3', _0: chroma, _1: x, _2: 0} : ((_elm_lang$core$Native_Utils.cmp(normHue, 2) < 0) ? {ctor: '_Tuple3', _0: x, _1: chroma, _2: 0} : ((_elm_lang$core$Native_Utils.cmp(normHue, 3) < 0) ? {ctor: '_Tuple3', _0: 0, _1: chroma, _2: x} : ((_elm_lang$core$Native_Utils.cmp(normHue, 4) < 0) ? {ctor: '_Tuple3', _0: 0, _1: x, _2: chroma} : ((_elm_lang$core$Native_Utils.cmp(normHue, 5) < 0) ? {ctor: '_Tuple3', _0: x, _1: 0, _2: chroma} : ((_elm_lang$core$Native_Utils.cmp(normHue, 6) < 0) ? {ctor: '_Tuple3', _0: chroma, _1: 0, _2: x} : {ctor: '_Tuple3', _0: 0, _1: 0, _2: 0}))))));
		var r = _p0._0;
		var g = _p0._1;
		var b = _p0._2;
		var m = lightness - (chroma / 2);
		return {ctor: '_Tuple3', _0: r + m, _1: g + m, _2: b + m};
	});
var _elm_lang$core$Color$toRgb = function (color) {
	var _p1 = color;
	if (_p1.ctor === 'RGBA') {
		return {red: _p1._0, green: _p1._1, blue: _p1._2, alpha: _p1._3};
	} else {
		var _p2 = A3(_elm_lang$core$Color$hslToRgb, _p1._0, _p1._1, _p1._2);
		var r = _p2._0;
		var g = _p2._1;
		var b = _p2._2;
		return {
			red: _elm_lang$core$Basics$round(255 * r),
			green: _elm_lang$core$Basics$round(255 * g),
			blue: _elm_lang$core$Basics$round(255 * b),
			alpha: _p1._3
		};
	}
};
var _elm_lang$core$Color$toHsl = function (color) {
	var _p3 = color;
	if (_p3.ctor === 'HSLA') {
		return {hue: _p3._0, saturation: _p3._1, lightness: _p3._2, alpha: _p3._3};
	} else {
		var _p4 = A3(_elm_lang$core$Color$rgbToHsl, _p3._0, _p3._1, _p3._2);
		var h = _p4._0;
		var s = _p4._1;
		var l = _p4._2;
		return {hue: h, saturation: s, lightness: l, alpha: _p3._3};
	}
};
var _elm_lang$core$Color$HSLA = F4(
	function (a, b, c, d) {
		return {ctor: 'HSLA', _0: a, _1: b, _2: c, _3: d};
	});
var _elm_lang$core$Color$hsla = F4(
	function (hue, saturation, lightness, alpha) {
		return A4(
			_elm_lang$core$Color$HSLA,
			hue - _elm_lang$core$Basics$turns(
				_elm_lang$core$Basics$toFloat(
					_elm_lang$core$Basics$floor(hue / (2 * _elm_lang$core$Basics$pi)))),
			saturation,
			lightness,
			alpha);
	});
var _elm_lang$core$Color$hsl = F3(
	function (hue, saturation, lightness) {
		return A4(_elm_lang$core$Color$hsla, hue, saturation, lightness, 1);
	});
var _elm_lang$core$Color$complement = function (color) {
	var _p5 = color;
	if (_p5.ctor === 'HSLA') {
		return A4(
			_elm_lang$core$Color$hsla,
			_p5._0 + _elm_lang$core$Basics$degrees(180),
			_p5._1,
			_p5._2,
			_p5._3);
	} else {
		var _p6 = A3(_elm_lang$core$Color$rgbToHsl, _p5._0, _p5._1, _p5._2);
		var h = _p6._0;
		var s = _p6._1;
		var l = _p6._2;
		return A4(
			_elm_lang$core$Color$hsla,
			h + _elm_lang$core$Basics$degrees(180),
			s,
			l,
			_p5._3);
	}
};
var _elm_lang$core$Color$grayscale = function (p) {
	return A4(_elm_lang$core$Color$HSLA, 0, 0, 1 - p, 1);
};
var _elm_lang$core$Color$greyscale = function (p) {
	return A4(_elm_lang$core$Color$HSLA, 0, 0, 1 - p, 1);
};
var _elm_lang$core$Color$RGBA = F4(
	function (a, b, c, d) {
		return {ctor: 'RGBA', _0: a, _1: b, _2: c, _3: d};
	});
var _elm_lang$core$Color$rgba = _elm_lang$core$Color$RGBA;
var _elm_lang$core$Color$rgb = F3(
	function (r, g, b) {
		return A4(_elm_lang$core$Color$RGBA, r, g, b, 1);
	});
var _elm_lang$core$Color$lightRed = A4(_elm_lang$core$Color$RGBA, 239, 41, 41, 1);
var _elm_lang$core$Color$red = A4(_elm_lang$core$Color$RGBA, 204, 0, 0, 1);
var _elm_lang$core$Color$darkRed = A4(_elm_lang$core$Color$RGBA, 164, 0, 0, 1);
var _elm_lang$core$Color$lightOrange = A4(_elm_lang$core$Color$RGBA, 252, 175, 62, 1);
var _elm_lang$core$Color$orange = A4(_elm_lang$core$Color$RGBA, 245, 121, 0, 1);
var _elm_lang$core$Color$darkOrange = A4(_elm_lang$core$Color$RGBA, 206, 92, 0, 1);
var _elm_lang$core$Color$lightYellow = A4(_elm_lang$core$Color$RGBA, 255, 233, 79, 1);
var _elm_lang$core$Color$yellow = A4(_elm_lang$core$Color$RGBA, 237, 212, 0, 1);
var _elm_lang$core$Color$darkYellow = A4(_elm_lang$core$Color$RGBA, 196, 160, 0, 1);
var _elm_lang$core$Color$lightGreen = A4(_elm_lang$core$Color$RGBA, 138, 226, 52, 1);
var _elm_lang$core$Color$green = A4(_elm_lang$core$Color$RGBA, 115, 210, 22, 1);
var _elm_lang$core$Color$darkGreen = A4(_elm_lang$core$Color$RGBA, 78, 154, 6, 1);
var _elm_lang$core$Color$lightBlue = A4(_elm_lang$core$Color$RGBA, 114, 159, 207, 1);
var _elm_lang$core$Color$blue = A4(_elm_lang$core$Color$RGBA, 52, 101, 164, 1);
var _elm_lang$core$Color$darkBlue = A4(_elm_lang$core$Color$RGBA, 32, 74, 135, 1);
var _elm_lang$core$Color$lightPurple = A4(_elm_lang$core$Color$RGBA, 173, 127, 168, 1);
var _elm_lang$core$Color$purple = A4(_elm_lang$core$Color$RGBA, 117, 80, 123, 1);
var _elm_lang$core$Color$darkPurple = A4(_elm_lang$core$Color$RGBA, 92, 53, 102, 1);
var _elm_lang$core$Color$lightBrown = A4(_elm_lang$core$Color$RGBA, 233, 185, 110, 1);
var _elm_lang$core$Color$brown = A4(_elm_lang$core$Color$RGBA, 193, 125, 17, 1);
var _elm_lang$core$Color$darkBrown = A4(_elm_lang$core$Color$RGBA, 143, 89, 2, 1);
var _elm_lang$core$Color$black = A4(_elm_lang$core$Color$RGBA, 0, 0, 0, 1);
var _elm_lang$core$Color$white = A4(_elm_lang$core$Color$RGBA, 255, 255, 255, 1);
var _elm_lang$core$Color$lightGrey = A4(_elm_lang$core$Color$RGBA, 238, 238, 236, 1);
var _elm_lang$core$Color$grey = A4(_elm_lang$core$Color$RGBA, 211, 215, 207, 1);
var _elm_lang$core$Color$darkGrey = A4(_elm_lang$core$Color$RGBA, 186, 189, 182, 1);
var _elm_lang$core$Color$lightGray = A4(_elm_lang$core$Color$RGBA, 238, 238, 236, 1);
var _elm_lang$core$Color$gray = A4(_elm_lang$core$Color$RGBA, 211, 215, 207, 1);
var _elm_lang$core$Color$darkGray = A4(_elm_lang$core$Color$RGBA, 186, 189, 182, 1);
var _elm_lang$core$Color$lightCharcoal = A4(_elm_lang$core$Color$RGBA, 136, 138, 133, 1);
var _elm_lang$core$Color$charcoal = A4(_elm_lang$core$Color$RGBA, 85, 87, 83, 1);
var _elm_lang$core$Color$darkCharcoal = A4(_elm_lang$core$Color$RGBA, 46, 52, 54, 1);
var _elm_lang$core$Color$Radial = F5(
	function (a, b, c, d, e) {
		return {ctor: 'Radial', _0: a, _1: b, _2: c, _3: d, _4: e};
	});
var _elm_lang$core$Color$radial = _elm_lang$core$Color$Radial;
var _elm_lang$core$Color$Linear = F3(
	function (a, b, c) {
		return {ctor: 'Linear', _0: a, _1: b, _2: c};
	});
var _elm_lang$core$Color$linear = _elm_lang$core$Color$Linear;

//import Native.Utils //

var _elm_lang$core$Native_Scheduler = function() {

var MAX_STEPS = 10000;


// TASKS

function succeed(value)
{
	return {
		ctor: '_Task_succeed',
		value: value
	};
}

function fail(error)
{
	return {
		ctor: '_Task_fail',
		value: error
	};
}

function nativeBinding(callback)
{
	return {
		ctor: '_Task_nativeBinding',
		callback: callback,
		cancel: null
	};
}

function andThen(callback, task)
{
	return {
		ctor: '_Task_andThen',
		callback: callback,
		task: task
	};
}

function onError(callback, task)
{
	return {
		ctor: '_Task_onError',
		callback: callback,
		task: task
	};
}

function receive(callback)
{
	return {
		ctor: '_Task_receive',
		callback: callback
	};
}


// PROCESSES

function rawSpawn(task)
{
	var process = {
		ctor: '_Process',
		id: _elm_lang$core$Native_Utils.guid(),
		root: task,
		stack: null,
		mailbox: []
	};

	enqueue(process);

	return process;
}

function spawn(task)
{
	return nativeBinding(function(callback) {
		var process = rawSpawn(task);
		callback(succeed(process));
	});
}

function rawSend(process, msg)
{
	process.mailbox.push(msg);
	enqueue(process);
}

function send(process, msg)
{
	return nativeBinding(function(callback) {
		rawSend(process, msg);
		callback(succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

function kill(process)
{
	return nativeBinding(function(callback) {
		var root = process.root;
		if (root.ctor === '_Task_nativeBinding' && root.cancel)
		{
			root.cancel();
		}

		process.root = null;

		callback(succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

function sleep(time)
{
	return nativeBinding(function(callback) {
		var id = setTimeout(function() {
			callback(succeed(_elm_lang$core$Native_Utils.Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}


// STEP PROCESSES

function step(numSteps, process)
{
	while (numSteps < MAX_STEPS)
	{
		var ctor = process.root.ctor;

		if (ctor === '_Task_succeed')
		{
			while (process.stack && process.stack.ctor === '_Task_onError')
			{
				process.stack = process.stack.rest;
			}
			if (process.stack === null)
			{
				break;
			}
			process.root = process.stack.callback(process.root.value);
			process.stack = process.stack.rest;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_fail')
		{
			while (process.stack && process.stack.ctor === '_Task_andThen')
			{
				process.stack = process.stack.rest;
			}
			if (process.stack === null)
			{
				break;
			}
			process.root = process.stack.callback(process.root.value);
			process.stack = process.stack.rest;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_andThen')
		{
			process.stack = {
				ctor: '_Task_andThen',
				callback: process.root.callback,
				rest: process.stack
			};
			process.root = process.root.task;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_onError')
		{
			process.stack = {
				ctor: '_Task_onError',
				callback: process.root.callback,
				rest: process.stack
			};
			process.root = process.root.task;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_nativeBinding')
		{
			process.root.cancel = process.root.callback(function(newRoot) {
				process.root = newRoot;
				enqueue(process);
			});

			break;
		}

		if (ctor === '_Task_receive')
		{
			var mailbox = process.mailbox;
			if (mailbox.length === 0)
			{
				break;
			}

			process.root = process.root.callback(mailbox.shift());
			++numSteps;
			continue;
		}

		throw new Error(ctor);
	}

	if (numSteps < MAX_STEPS)
	{
		return numSteps + 1;
	}
	enqueue(process);

	return numSteps;
}


// WORK QUEUE

var working = false;
var workQueue = [];

function enqueue(process)
{
	workQueue.push(process);

	if (!working)
	{
		setTimeout(work, 0);
		working = true;
	}
}

function work()
{
	var numSteps = 0;
	var process;
	while (numSteps < MAX_STEPS && (process = workQueue.shift()))
	{
		if (process.root)
		{
			numSteps = step(numSteps, process);
		}
	}
	if (!process)
	{
		working = false;
		return;
	}
	setTimeout(work, 0);
}


return {
	succeed: succeed,
	fail: fail,
	nativeBinding: nativeBinding,
	andThen: F2(andThen),
	onError: F2(onError),
	receive: receive,

	spawn: spawn,
	kill: kill,
	sleep: sleep,
	send: F2(send),

	rawSpawn: rawSpawn,
	rawSend: rawSend
};

}();
//import //

var _elm_lang$core$Native_Platform = function() {


// PROGRAMS

function program(impl)
{
	return function(flagDecoder)
	{
		return function(object, moduleName)
		{
			object['worker'] = function worker(flags)
			{
				if (typeof flags !== 'undefined')
				{
					throw new Error(
						'The `' + moduleName + '` module does not need flags.\n'
						+ 'Call ' + moduleName + '.worker() with no arguments and you should be all set!'
					);
				}

				return initialize(
					impl.init,
					impl.update,
					impl.subscriptions,
					renderer
				);
			};
		};
	};
}

function programWithFlags(impl)
{
	return function(flagDecoder)
	{
		return function(object, moduleName)
		{
			object['worker'] = function worker(flags)
			{
				if (typeof flagDecoder === 'undefined')
				{
					throw new Error(
						'Are you trying to sneak a Never value into Elm? Trickster!\n'
						+ 'It looks like ' + moduleName + '.main is defined with `programWithFlags` but has type `Program Never`.\n'
						+ 'Use `program` instead if you do not want flags.'
					);
				}

				var result = A2(_elm_lang$core$Native_Json.run, flagDecoder, flags);
				if (result.ctor === 'Err')
				{
					throw new Error(
						moduleName + '.worker(...) was called with an unexpected argument.\n'
						+ 'I tried to convert it to an Elm value, but ran into this problem:\n\n'
						+ result._0
					);
				}

				return initialize(
					impl.init(result._0),
					impl.update,
					impl.subscriptions,
					renderer
				);
			};
		};
	};
}

function renderer(enqueue, _)
{
	return function(_) {};
}


// HTML TO PROGRAM

function htmlToProgram(vnode)
{
	var emptyBag = batch(_elm_lang$core$Native_List.Nil);
	var noChange = _elm_lang$core$Native_Utils.Tuple2(
		_elm_lang$core$Native_Utils.Tuple0,
		emptyBag
	);

	return _elm_lang$virtual_dom$VirtualDom$program({
		init: noChange,
		view: function(model) { return main; },
		update: F2(function(msg, model) { return noChange; }),
		subscriptions: function (model) { return emptyBag; }
	});
}


// INITIALIZE A PROGRAM

function initialize(init, update, subscriptions, renderer)
{
	// ambient state
	var managers = {};
	var updateView;

	// init and update state in main process
	var initApp = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {
		var model = init._0;
		updateView = renderer(enqueue, model);
		var cmds = init._1;
		var subs = subscriptions(model);
		dispatchEffects(managers, cmds, subs);
		callback(_elm_lang$core$Native_Scheduler.succeed(model));
	});

	function onMessage(msg, model)
	{
		return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {
			var results = A2(update, msg, model);
			model = results._0;
			updateView(model);
			var cmds = results._1;
			var subs = subscriptions(model);
			dispatchEffects(managers, cmds, subs);
			callback(_elm_lang$core$Native_Scheduler.succeed(model));
		});
	}

	var mainProcess = spawnLoop(initApp, onMessage);

	function enqueue(msg)
	{
		_elm_lang$core$Native_Scheduler.rawSend(mainProcess, msg);
	}

	var ports = setupEffects(managers, enqueue);

	return ports ? { ports: ports } : {};
}


// EFFECT MANAGERS

var effectManagers = {};

function setupEffects(managers, callback)
{
	var ports;

	// setup all necessary effect managers
	for (var key in effectManagers)
	{
		var manager = effectManagers[key];

		if (manager.isForeign)
		{
			ports = ports || {};
			ports[key] = manager.tag === 'cmd'
				? setupOutgoingPort(key)
				: setupIncomingPort(key, callback);
		}

		managers[key] = makeManager(manager, callback);
	}

	return ports;
}

function makeManager(info, callback)
{
	var router = {
		main: callback,
		self: undefined
	};

	var tag = info.tag;
	var onEffects = info.onEffects;
	var onSelfMsg = info.onSelfMsg;

	function onMessage(msg, state)
	{
		if (msg.ctor === 'self')
		{
			return A3(onSelfMsg, router, msg._0, state);
		}

		var fx = msg._0;
		switch (tag)
		{
			case 'cmd':
				return A3(onEffects, router, fx.cmds, state);

			case 'sub':
				return A3(onEffects, router, fx.subs, state);

			case 'fx':
				return A4(onEffects, router, fx.cmds, fx.subs, state);
		}
	}

	var process = spawnLoop(info.init, onMessage);
	router.self = process;
	return process;
}

function sendToApp(router, msg)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		router.main(msg);
		callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

function sendToSelf(router, msg)
{
	return A2(_elm_lang$core$Native_Scheduler.send, router.self, {
		ctor: 'self',
		_0: msg
	});
}


// HELPER for STATEFUL LOOPS

function spawnLoop(init, onMessage)
{
	var andThen = _elm_lang$core$Native_Scheduler.andThen;

	function loop(state)
	{
		var handleMsg = _elm_lang$core$Native_Scheduler.receive(function(msg) {
			return onMessage(msg, state);
		});
		return A2(andThen, loop, handleMsg);
	}

	var task = A2(andThen, loop, init);

	return _elm_lang$core$Native_Scheduler.rawSpawn(task);
}


// BAGS

function leaf(home)
{
	return function(value)
	{
		return {
			type: 'leaf',
			home: home,
			value: value
		};
	};
}

function batch(list)
{
	return {
		type: 'node',
		branches: list
	};
}

function map(tagger, bag)
{
	return {
		type: 'map',
		tagger: tagger,
		tree: bag
	}
}


// PIPE BAGS INTO EFFECT MANAGERS

function dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	gatherEffects(true, cmdBag, effectsDict, null);
	gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		var fx = home in effectsDict
			? effectsDict[home]
			: {
				cmds: _elm_lang$core$Native_List.Nil,
				subs: _elm_lang$core$Native_List.Nil
			};

		_elm_lang$core$Native_Scheduler.rawSend(managers[home], { ctor: 'fx', _0: fx });
	}
}

function gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.type)
	{
		case 'leaf':
			var home = bag.home;
			var effect = toEffect(isCmd, home, taggers, bag.value);
			effectsDict[home] = insert(isCmd, effect, effectsDict[home]);
			return;

		case 'node':
			var list = bag.branches;
			while (list.ctor !== '[]')
			{
				gatherEffects(isCmd, list._0, effectsDict, taggers);
				list = list._1;
			}
			return;

		case 'map':
			gatherEffects(isCmd, bag.tree, effectsDict, {
				tagger: bag.tagger,
				rest: taggers
			});
			return;
	}
}

function toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		var temp = taggers;
		while (temp)
		{
			x = temp.tagger(x);
			temp = temp.rest;
		}
		return x;
	}

	var map = isCmd
		? effectManagers[home].cmdMap
		: effectManagers[home].subMap;

	return A2(map, applyTaggers, value)
}

function insert(isCmd, newEffect, effects)
{
	effects = effects || {
		cmds: _elm_lang$core$Native_List.Nil,
		subs: _elm_lang$core$Native_List.Nil
	};
	if (isCmd)
	{
		effects.cmds = _elm_lang$core$Native_List.Cons(newEffect, effects.cmds);
		return effects;
	}
	effects.subs = _elm_lang$core$Native_List.Cons(newEffect, effects.subs);
	return effects;
}


// PORTS

function checkPortName(name)
{
	if (name in effectManagers)
	{
		throw new Error('There can only be one port named `' + name + '`, but your program has multiple.');
	}
}


// OUTGOING PORTS

function outgoingPort(name, converter)
{
	checkPortName(name);
	effectManagers[name] = {
		tag: 'cmd',
		cmdMap: outgoingPortMap,
		converter: converter,
		isForeign: true
	};
	return leaf(name);
}

var outgoingPortMap = F2(function cmdMap(tagger, value) {
	return value;
});

function setupOutgoingPort(name)
{
	var subs = [];
	var converter = effectManagers[name].converter;

	// CREATE MANAGER

	var init = _elm_lang$core$Native_Scheduler.succeed(null);

	function onEffects(router, cmdList, state)
	{
		while (cmdList.ctor !== '[]')
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = converter(cmdList._0);
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
			cmdList = cmdList._1;
		}
		return init;
	}

	effectManagers[name].init = init;
	effectManagers[name].onEffects = F3(onEffects);

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}


// INCOMING PORTS

function incomingPort(name, converter)
{
	checkPortName(name);
	effectManagers[name] = {
		tag: 'sub',
		subMap: incomingPortMap,
		converter: converter,
		isForeign: true
	};
	return leaf(name);
}

var incomingPortMap = F2(function subMap(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});

function setupIncomingPort(name, callback)
{
	var sentBeforeInit = [];
	var subs = _elm_lang$core$Native_List.Nil;
	var converter = effectManagers[name].converter;
	var currentOnEffects = preInitOnEffects;
	var currentSend = preInitSend;

	// CREATE MANAGER

	var init = _elm_lang$core$Native_Scheduler.succeed(null);

	function preInitOnEffects(router, subList, state)
	{
		var postInitResult = postInitOnEffects(router, subList, state);

		for(var i = 0; i < sentBeforeInit.length; i++)
		{
			postInitSend(sentBeforeInit[i]);
		}

		sentBeforeInit = null; // to release objects held in queue
		currentSend = postInitSend;
		currentOnEffects = postInitOnEffects;
		return postInitResult;
	}

	function postInitOnEffects(router, subList, state)
	{
		subs = subList;
		return init;
	}

	function onEffects(router, subList, state)
	{
		return currentOnEffects(router, subList, state);
	}

	effectManagers[name].init = init;
	effectManagers[name].onEffects = F3(onEffects);

	// PUBLIC API

	function preInitSend(value)
	{
		sentBeforeInit.push(value);
	}

	function postInitSend(value)
	{
		var temp = subs;
		while (temp.ctor !== '[]')
		{
			callback(temp._0(value));
			temp = temp._1;
		}
	}

	function send(incomingValue)
	{
		var result = A2(_elm_lang$core$Json_Decode$decodeValue, converter, incomingValue);
		if (result.ctor === 'Err')
		{
			throw new Error('Trying to send an unexpected type of value through port `' + name + '`:\n' + result._0);
		}

		currentSend(result._0);
	}

	return { send: send };
}

return {
	// routers
	sendToApp: F2(sendToApp),
	sendToSelf: F2(sendToSelf),

	// global setup
	effectManagers: effectManagers,
	outgoingPort: outgoingPort,
	incomingPort: incomingPort,

	htmlToProgram: htmlToProgram,
	program: program,
	programWithFlags: programWithFlags,
	initialize: initialize,

	// effect bags
	leaf: leaf,
	batch: batch,
	map: F2(map)
};

}();

var _elm_lang$core$Platform_Cmd$batch = _elm_lang$core$Native_Platform.batch;
var _elm_lang$core$Platform_Cmd$none = _elm_lang$core$Platform_Cmd$batch(
	{ctor: '[]'});
var _elm_lang$core$Platform_Cmd_ops = _elm_lang$core$Platform_Cmd_ops || {};
_elm_lang$core$Platform_Cmd_ops['!'] = F2(
	function (model, commands) {
		return {
			ctor: '_Tuple2',
			_0: model,
			_1: _elm_lang$core$Platform_Cmd$batch(commands)
		};
	});
var _elm_lang$core$Platform_Cmd$map = _elm_lang$core$Native_Platform.map;
var _elm_lang$core$Platform_Cmd$Cmd = {ctor: 'Cmd'};

var _elm_lang$core$Platform_Sub$batch = _elm_lang$core$Native_Platform.batch;
var _elm_lang$core$Platform_Sub$none = _elm_lang$core$Platform_Sub$batch(
	{ctor: '[]'});
var _elm_lang$core$Platform_Sub$map = _elm_lang$core$Native_Platform.map;
var _elm_lang$core$Platform_Sub$Sub = {ctor: 'Sub'};

var _elm_lang$core$Platform$hack = _elm_lang$core$Native_Scheduler.succeed;
var _elm_lang$core$Platform$sendToSelf = _elm_lang$core$Native_Platform.sendToSelf;
var _elm_lang$core$Platform$sendToApp = _elm_lang$core$Native_Platform.sendToApp;
var _elm_lang$core$Platform$programWithFlags = _elm_lang$core$Native_Platform.programWithFlags;
var _elm_lang$core$Platform$program = _elm_lang$core$Native_Platform.program;
var _elm_lang$core$Platform$Program = {ctor: 'Program'};
var _elm_lang$core$Platform$Task = {ctor: 'Task'};
var _elm_lang$core$Platform$ProcessId = {ctor: 'ProcessId'};
var _elm_lang$core$Platform$Router = {ctor: 'Router'};

var _elm_lang$core$Result$toMaybe = function (result) {
	var _p0 = result;
	if (_p0.ctor === 'Ok') {
		return _elm_lang$core$Maybe$Just(_p0._0);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$Result$withDefault = F2(
	function (def, result) {
		var _p1 = result;
		if (_p1.ctor === 'Ok') {
			return _p1._0;
		} else {
			return def;
		}
	});
var _elm_lang$core$Result$Err = function (a) {
	return {ctor: 'Err', _0: a};
};
var _elm_lang$core$Result$andThen = F2(
	function (callback, result) {
		var _p2 = result;
		if (_p2.ctor === 'Ok') {
			return callback(_p2._0);
		} else {
			return _elm_lang$core$Result$Err(_p2._0);
		}
	});
var _elm_lang$core$Result$Ok = function (a) {
	return {ctor: 'Ok', _0: a};
};
var _elm_lang$core$Result$map = F2(
	function (func, ra) {
		var _p3 = ra;
		if (_p3.ctor === 'Ok') {
			return _elm_lang$core$Result$Ok(
				func(_p3._0));
		} else {
			return _elm_lang$core$Result$Err(_p3._0);
		}
	});
var _elm_lang$core$Result$map2 = F3(
	function (func, ra, rb) {
		var _p4 = {ctor: '_Tuple2', _0: ra, _1: rb};
		if (_p4._0.ctor === 'Ok') {
			if (_p4._1.ctor === 'Ok') {
				return _elm_lang$core$Result$Ok(
					A2(func, _p4._0._0, _p4._1._0));
			} else {
				return _elm_lang$core$Result$Err(_p4._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p4._0._0);
		}
	});
var _elm_lang$core$Result$map3 = F4(
	function (func, ra, rb, rc) {
		var _p5 = {ctor: '_Tuple3', _0: ra, _1: rb, _2: rc};
		if (_p5._0.ctor === 'Ok') {
			if (_p5._1.ctor === 'Ok') {
				if (_p5._2.ctor === 'Ok') {
					return _elm_lang$core$Result$Ok(
						A3(func, _p5._0._0, _p5._1._0, _p5._2._0));
				} else {
					return _elm_lang$core$Result$Err(_p5._2._0);
				}
			} else {
				return _elm_lang$core$Result$Err(_p5._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p5._0._0);
		}
	});
var _elm_lang$core$Result$map4 = F5(
	function (func, ra, rb, rc, rd) {
		var _p6 = {ctor: '_Tuple4', _0: ra, _1: rb, _2: rc, _3: rd};
		if (_p6._0.ctor === 'Ok') {
			if (_p6._1.ctor === 'Ok') {
				if (_p6._2.ctor === 'Ok') {
					if (_p6._3.ctor === 'Ok') {
						return _elm_lang$core$Result$Ok(
							A4(func, _p6._0._0, _p6._1._0, _p6._2._0, _p6._3._0));
					} else {
						return _elm_lang$core$Result$Err(_p6._3._0);
					}
				} else {
					return _elm_lang$core$Result$Err(_p6._2._0);
				}
			} else {
				return _elm_lang$core$Result$Err(_p6._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p6._0._0);
		}
	});
var _elm_lang$core$Result$map5 = F6(
	function (func, ra, rb, rc, rd, re) {
		var _p7 = {ctor: '_Tuple5', _0: ra, _1: rb, _2: rc, _3: rd, _4: re};
		if (_p7._0.ctor === 'Ok') {
			if (_p7._1.ctor === 'Ok') {
				if (_p7._2.ctor === 'Ok') {
					if (_p7._3.ctor === 'Ok') {
						if (_p7._4.ctor === 'Ok') {
							return _elm_lang$core$Result$Ok(
								A5(func, _p7._0._0, _p7._1._0, _p7._2._0, _p7._3._0, _p7._4._0));
						} else {
							return _elm_lang$core$Result$Err(_p7._4._0);
						}
					} else {
						return _elm_lang$core$Result$Err(_p7._3._0);
					}
				} else {
					return _elm_lang$core$Result$Err(_p7._2._0);
				}
			} else {
				return _elm_lang$core$Result$Err(_p7._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p7._0._0);
		}
	});
var _elm_lang$core$Result$mapError = F2(
	function (f, result) {
		var _p8 = result;
		if (_p8.ctor === 'Ok') {
			return _elm_lang$core$Result$Ok(_p8._0);
		} else {
			return _elm_lang$core$Result$Err(
				f(_p8._0));
		}
	});
var _elm_lang$core$Result$fromMaybe = F2(
	function (err, maybe) {
		var _p9 = maybe;
		if (_p9.ctor === 'Just') {
			return _elm_lang$core$Result$Ok(_p9._0);
		} else {
			return _elm_lang$core$Result$Err(err);
		}
	});

var _elm_lang$core$Task$onError = _elm_lang$core$Native_Scheduler.onError;
var _elm_lang$core$Task$andThen = _elm_lang$core$Native_Scheduler.andThen;
var _elm_lang$core$Task$spawnCmd = F2(
	function (router, _p0) {
		var _p1 = _p0;
		return _elm_lang$core$Native_Scheduler.spawn(
			A2(
				_elm_lang$core$Task$andThen,
				_elm_lang$core$Platform$sendToApp(router),
				_p1._0));
	});
var _elm_lang$core$Task$fail = _elm_lang$core$Native_Scheduler.fail;
var _elm_lang$core$Task$mapError = F2(
	function (convert, task) {
		return A2(
			_elm_lang$core$Task$onError,
			function (_p2) {
				return _elm_lang$core$Task$fail(
					convert(_p2));
			},
			task);
	});
var _elm_lang$core$Task$succeed = _elm_lang$core$Native_Scheduler.succeed;
var _elm_lang$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return _elm_lang$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var _elm_lang$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return _elm_lang$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$map3 = F4(
	function (func, taskA, taskB, taskC) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (c) {
								return _elm_lang$core$Task$succeed(
									A3(func, a, b, c));
							},
							taskC);
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$map4 = F5(
	function (func, taskA, taskB, taskC, taskD) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (c) {
								return A2(
									_elm_lang$core$Task$andThen,
									function (d) {
										return _elm_lang$core$Task$succeed(
											A4(func, a, b, c, d));
									},
									taskD);
							},
							taskC);
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$map5 = F6(
	function (func, taskA, taskB, taskC, taskD, taskE) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (c) {
								return A2(
									_elm_lang$core$Task$andThen,
									function (d) {
										return A2(
											_elm_lang$core$Task$andThen,
											function (e) {
												return _elm_lang$core$Task$succeed(
													A5(func, a, b, c, d, e));
											},
											taskE);
									},
									taskD);
							},
							taskC);
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$sequence = function (tasks) {
	var _p3 = tasks;
	if (_p3.ctor === '[]') {
		return _elm_lang$core$Task$succeed(
			{ctor: '[]'});
	} else {
		return A3(
			_elm_lang$core$Task$map2,
			F2(
				function (x, y) {
					return {ctor: '::', _0: x, _1: y};
				}),
			_p3._0,
			_elm_lang$core$Task$sequence(_p3._1));
	}
};
var _elm_lang$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			_elm_lang$core$Task$map,
			function (_p4) {
				return {ctor: '_Tuple0'};
			},
			_elm_lang$core$Task$sequence(
				A2(
					_elm_lang$core$List$map,
					_elm_lang$core$Task$spawnCmd(router),
					commands)));
	});
var _elm_lang$core$Task$init = _elm_lang$core$Task$succeed(
	{ctor: '_Tuple0'});
var _elm_lang$core$Task$onSelfMsg = F3(
	function (_p7, _p6, _p5) {
		return _elm_lang$core$Task$succeed(
			{ctor: '_Tuple0'});
	});
var _elm_lang$core$Task$command = _elm_lang$core$Native_Platform.leaf('Task');
var _elm_lang$core$Task$Perform = function (a) {
	return {ctor: 'Perform', _0: a};
};
var _elm_lang$core$Task$perform = F2(
	function (toMessage, task) {
		return _elm_lang$core$Task$command(
			_elm_lang$core$Task$Perform(
				A2(_elm_lang$core$Task$map, toMessage, task)));
	});
var _elm_lang$core$Task$attempt = F2(
	function (resultToMessage, task) {
		return _elm_lang$core$Task$command(
			_elm_lang$core$Task$Perform(
				A2(
					_elm_lang$core$Task$onError,
					function (_p8) {
						return _elm_lang$core$Task$succeed(
							resultToMessage(
								_elm_lang$core$Result$Err(_p8)));
					},
					A2(
						_elm_lang$core$Task$andThen,
						function (_p9) {
							return _elm_lang$core$Task$succeed(
								resultToMessage(
									_elm_lang$core$Result$Ok(_p9)));
						},
						task))));
	});
var _elm_lang$core$Task$cmdMap = F2(
	function (tagger, _p10) {
		var _p11 = _p10;
		return _elm_lang$core$Task$Perform(
			A2(_elm_lang$core$Task$map, tagger, _p11._0));
	});
_elm_lang$core$Native_Platform.effectManagers['Task'] = {pkg: 'elm-lang/core', init: _elm_lang$core$Task$init, onEffects: _elm_lang$core$Task$onEffects, onSelfMsg: _elm_lang$core$Task$onSelfMsg, tag: 'cmd', cmdMap: _elm_lang$core$Task$cmdMap};

//import Native.Utils //

var _elm_lang$core$Native_Debug = function() {

function log(tag, value)
{
	var msg = tag + ': ' + _elm_lang$core$Native_Utils.toString(value);
	var process = process || {};
	if (process.stdout)
	{
		process.stdout.write(msg);
	}
	else
	{
		console.log(msg);
	}
	return value;
}

function crash(message)
{
	throw new Error(message);
}

return {
	crash: crash,
	log: F2(log)
};

}();
//import Maybe, Native.List, Native.Utils, Result //

var _elm_lang$core$Native_String = function() {

function isEmpty(str)
{
	return str.length === 0;
}
function cons(chr, str)
{
	return chr + str;
}
function uncons(str)
{
	var hd = str[0];
	if (hd)
	{
		return _elm_lang$core$Maybe$Just(_elm_lang$core$Native_Utils.Tuple2(_elm_lang$core$Native_Utils.chr(hd), str.slice(1)));
	}
	return _elm_lang$core$Maybe$Nothing;
}
function append(a, b)
{
	return a + b;
}
function concat(strs)
{
	return _elm_lang$core$Native_List.toArray(strs).join('');
}
function length(str)
{
	return str.length;
}
function map(f, str)
{
	var out = str.split('');
	for (var i = out.length; i--; )
	{
		out[i] = f(_elm_lang$core$Native_Utils.chr(out[i]));
	}
	return out.join('');
}
function filter(pred, str)
{
	return str.split('').map(_elm_lang$core$Native_Utils.chr).filter(pred).join('');
}
function reverse(str)
{
	return str.split('').reverse().join('');
}
function foldl(f, b, str)
{
	var len = str.length;
	for (var i = 0; i < len; ++i)
	{
		b = A2(f, _elm_lang$core$Native_Utils.chr(str[i]), b);
	}
	return b;
}
function foldr(f, b, str)
{
	for (var i = str.length; i--; )
	{
		b = A2(f, _elm_lang$core$Native_Utils.chr(str[i]), b);
	}
	return b;
}
function split(sep, str)
{
	return _elm_lang$core$Native_List.fromArray(str.split(sep));
}
function join(sep, strs)
{
	return _elm_lang$core$Native_List.toArray(strs).join(sep);
}
function repeat(n, str)
{
	var result = '';
	while (n > 0)
	{
		if (n & 1)
		{
			result += str;
		}
		n >>= 1, str += str;
	}
	return result;
}
function slice(start, end, str)
{
	return str.slice(start, end);
}
function left(n, str)
{
	return n < 1 ? '' : str.slice(0, n);
}
function right(n, str)
{
	return n < 1 ? '' : str.slice(-n);
}
function dropLeft(n, str)
{
	return n < 1 ? str : str.slice(n);
}
function dropRight(n, str)
{
	return n < 1 ? str : str.slice(0, -n);
}
function pad(n, chr, str)
{
	var half = (n - str.length) / 2;
	return repeat(Math.ceil(half), chr) + str + repeat(half | 0, chr);
}
function padRight(n, chr, str)
{
	return str + repeat(n - str.length, chr);
}
function padLeft(n, chr, str)
{
	return repeat(n - str.length, chr) + str;
}

function trim(str)
{
	return str.trim();
}
function trimLeft(str)
{
	return str.replace(/^\s+/, '');
}
function trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function words(str)
{
	return _elm_lang$core$Native_List.fromArray(str.trim().split(/\s+/g));
}
function lines(str)
{
	return _elm_lang$core$Native_List.fromArray(str.split(/\r\n|\r|\n/g));
}

function toUpper(str)
{
	return str.toUpperCase();
}
function toLower(str)
{
	return str.toLowerCase();
}

function any(pred, str)
{
	for (var i = str.length; i--; )
	{
		if (pred(_elm_lang$core$Native_Utils.chr(str[i])))
		{
			return true;
		}
	}
	return false;
}
function all(pred, str)
{
	for (var i = str.length; i--; )
	{
		if (!pred(_elm_lang$core$Native_Utils.chr(str[i])))
		{
			return false;
		}
	}
	return true;
}

function contains(sub, str)
{
	return str.indexOf(sub) > -1;
}
function startsWith(sub, str)
{
	return str.indexOf(sub) === 0;
}
function endsWith(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
}
function indexes(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _elm_lang$core$Native_List.Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _elm_lang$core$Native_List.fromArray(is);
}


function toInt(s)
{
	var len = s.length;

	// if empty
	if (len === 0)
	{
		return intErr(s);
	}

	// if hex
	var c = s[0];
	if (c === '0' && s[1] === 'x')
	{
		for (var i = 2; i < len; ++i)
		{
			var c = s[i];
			if (('0' <= c && c <= '9') || ('A' <= c && c <= 'F') || ('a' <= c && c <= 'f'))
			{
				continue;
			}
			return intErr(s);
		}
		return _elm_lang$core$Result$Ok(parseInt(s, 16));
	}

	// is decimal
	if (c > '9' || (c < '0' && c !== '-' && c !== '+'))
	{
		return intErr(s);
	}
	for (var i = 1; i < len; ++i)
	{
		var c = s[i];
		if (c < '0' || '9' < c)
		{
			return intErr(s);
		}
	}

	return _elm_lang$core$Result$Ok(parseInt(s, 10));
}

function intErr(s)
{
	return _elm_lang$core$Result$Err("could not convert string '" + s + "' to an Int");
}


function toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return floatErr(s);
	}
	var n = +s;
	// faster isNaN check
	return n === n ? _elm_lang$core$Result$Ok(n) : floatErr(s);
}

function floatErr(s)
{
	return _elm_lang$core$Result$Err("could not convert string '" + s + "' to a Float");
}


function toList(str)
{
	return _elm_lang$core$Native_List.fromArray(str.split('').map(_elm_lang$core$Native_Utils.chr));
}
function fromList(chars)
{
	return _elm_lang$core$Native_List.toArray(chars).join('');
}

return {
	isEmpty: isEmpty,
	cons: F2(cons),
	uncons: uncons,
	append: F2(append),
	concat: concat,
	length: length,
	map: F2(map),
	filter: F2(filter),
	reverse: reverse,
	foldl: F3(foldl),
	foldr: F3(foldr),

	split: F2(split),
	join: F2(join),
	repeat: F2(repeat),

	slice: F3(slice),
	left: F2(left),
	right: F2(right),
	dropLeft: F2(dropLeft),
	dropRight: F2(dropRight),

	pad: F3(pad),
	padLeft: F3(padLeft),
	padRight: F3(padRight),

	trim: trim,
	trimLeft: trimLeft,
	trimRight: trimRight,

	words: words,
	lines: lines,

	toUpper: toUpper,
	toLower: toLower,

	any: F2(any),
	all: F2(all),

	contains: F2(contains),
	startsWith: F2(startsWith),
	endsWith: F2(endsWith),
	indexes: F2(indexes),

	toInt: toInt,
	toFloat: toFloat,
	toList: toList,
	fromList: fromList
};

}();

var _elm_lang$core$String$fromList = _elm_lang$core$Native_String.fromList;
var _elm_lang$core$String$toList = _elm_lang$core$Native_String.toList;
var _elm_lang$core$String$toFloat = _elm_lang$core$Native_String.toFloat;
var _elm_lang$core$String$toInt = _elm_lang$core$Native_String.toInt;
var _elm_lang$core$String$indices = _elm_lang$core$Native_String.indexes;
var _elm_lang$core$String$indexes = _elm_lang$core$Native_String.indexes;
var _elm_lang$core$String$endsWith = _elm_lang$core$Native_String.endsWith;
var _elm_lang$core$String$startsWith = _elm_lang$core$Native_String.startsWith;
var _elm_lang$core$String$contains = _elm_lang$core$Native_String.contains;
var _elm_lang$core$String$all = _elm_lang$core$Native_String.all;
var _elm_lang$core$String$any = _elm_lang$core$Native_String.any;
var _elm_lang$core$String$toLower = _elm_lang$core$Native_String.toLower;
var _elm_lang$core$String$toUpper = _elm_lang$core$Native_String.toUpper;
var _elm_lang$core$String$lines = _elm_lang$core$Native_String.lines;
var _elm_lang$core$String$words = _elm_lang$core$Native_String.words;
var _elm_lang$core$String$trimRight = _elm_lang$core$Native_String.trimRight;
var _elm_lang$core$String$trimLeft = _elm_lang$core$Native_String.trimLeft;
var _elm_lang$core$String$trim = _elm_lang$core$Native_String.trim;
var _elm_lang$core$String$padRight = _elm_lang$core$Native_String.padRight;
var _elm_lang$core$String$padLeft = _elm_lang$core$Native_String.padLeft;
var _elm_lang$core$String$pad = _elm_lang$core$Native_String.pad;
var _elm_lang$core$String$dropRight = _elm_lang$core$Native_String.dropRight;
var _elm_lang$core$String$dropLeft = _elm_lang$core$Native_String.dropLeft;
var _elm_lang$core$String$right = _elm_lang$core$Native_String.right;
var _elm_lang$core$String$left = _elm_lang$core$Native_String.left;
var _elm_lang$core$String$slice = _elm_lang$core$Native_String.slice;
var _elm_lang$core$String$repeat = _elm_lang$core$Native_String.repeat;
var _elm_lang$core$String$join = _elm_lang$core$Native_String.join;
var _elm_lang$core$String$split = _elm_lang$core$Native_String.split;
var _elm_lang$core$String$foldr = _elm_lang$core$Native_String.foldr;
var _elm_lang$core$String$foldl = _elm_lang$core$Native_String.foldl;
var _elm_lang$core$String$reverse = _elm_lang$core$Native_String.reverse;
var _elm_lang$core$String$filter = _elm_lang$core$Native_String.filter;
var _elm_lang$core$String$map = _elm_lang$core$Native_String.map;
var _elm_lang$core$String$length = _elm_lang$core$Native_String.length;
var _elm_lang$core$String$concat = _elm_lang$core$Native_String.concat;
var _elm_lang$core$String$append = _elm_lang$core$Native_String.append;
var _elm_lang$core$String$uncons = _elm_lang$core$Native_String.uncons;
var _elm_lang$core$String$cons = _elm_lang$core$Native_String.cons;
var _elm_lang$core$String$fromChar = function ($char) {
	return A2(_elm_lang$core$String$cons, $char, '');
};
var _elm_lang$core$String$isEmpty = _elm_lang$core$Native_String.isEmpty;

var _elm_lang$core$Dict$foldr = F3(
	function (f, acc, t) {
		foldr:
		while (true) {
			var _p0 = t;
			if (_p0.ctor === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var _v1 = f,
					_v2 = A3(
					f,
					_p0._1,
					_p0._2,
					A3(_elm_lang$core$Dict$foldr, f, acc, _p0._4)),
					_v3 = _p0._3;
				f = _v1;
				acc = _v2;
				t = _v3;
				continue foldr;
			}
		}
	});
var _elm_lang$core$Dict$keys = function (dict) {
	return A3(
		_elm_lang$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return {ctor: '::', _0: key, _1: keyList};
			}),
		{ctor: '[]'},
		dict);
};
var _elm_lang$core$Dict$values = function (dict) {
	return A3(
		_elm_lang$core$Dict$foldr,
		F3(
			function (key, value, valueList) {
				return {ctor: '::', _0: value, _1: valueList};
			}),
		{ctor: '[]'},
		dict);
};
var _elm_lang$core$Dict$toList = function (dict) {
	return A3(
		_elm_lang$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: key, _1: value},
					_1: list
				};
			}),
		{ctor: '[]'},
		dict);
};
var _elm_lang$core$Dict$foldl = F3(
	function (f, acc, dict) {
		foldl:
		while (true) {
			var _p1 = dict;
			if (_p1.ctor === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var _v5 = f,
					_v6 = A3(
					f,
					_p1._1,
					_p1._2,
					A3(_elm_lang$core$Dict$foldl, f, acc, _p1._3)),
					_v7 = _p1._4;
				f = _v5;
				acc = _v6;
				dict = _v7;
				continue foldl;
			}
		}
	});
var _elm_lang$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _p2) {
				stepState:
				while (true) {
					var _p3 = _p2;
					var _p9 = _p3._1;
					var _p8 = _p3._0;
					var _p4 = _p8;
					if (_p4.ctor === '[]') {
						return {
							ctor: '_Tuple2',
							_0: _p8,
							_1: A3(rightStep, rKey, rValue, _p9)
						};
					} else {
						var _p7 = _p4._1;
						var _p6 = _p4._0._1;
						var _p5 = _p4._0._0;
						if (_elm_lang$core$Native_Utils.cmp(_p5, rKey) < 0) {
							var _v10 = rKey,
								_v11 = rValue,
								_v12 = {
								ctor: '_Tuple2',
								_0: _p7,
								_1: A3(leftStep, _p5, _p6, _p9)
							};
							rKey = _v10;
							rValue = _v11;
							_p2 = _v12;
							continue stepState;
						} else {
							if (_elm_lang$core$Native_Utils.cmp(_p5, rKey) > 0) {
								return {
									ctor: '_Tuple2',
									_0: _p8,
									_1: A3(rightStep, rKey, rValue, _p9)
								};
							} else {
								return {
									ctor: '_Tuple2',
									_0: _p7,
									_1: A4(bothStep, _p5, _p6, rValue, _p9)
								};
							}
						}
					}
				}
			});
		var _p10 = A3(
			_elm_lang$core$Dict$foldl,
			stepState,
			{
				ctor: '_Tuple2',
				_0: _elm_lang$core$Dict$toList(leftDict),
				_1: initialResult
			},
			rightDict);
		var leftovers = _p10._0;
		var intermediateResult = _p10._1;
		return A3(
			_elm_lang$core$List$foldl,
			F2(
				function (_p11, result) {
					var _p12 = _p11;
					return A3(leftStep, _p12._0, _p12._1, result);
				}),
			intermediateResult,
			leftovers);
	});
var _elm_lang$core$Dict$reportRemBug = F4(
	function (msg, c, lgot, rgot) {
		return _elm_lang$core$Native_Debug.crash(
			_elm_lang$core$String$concat(
				{
					ctor: '::',
					_0: 'Internal red-black tree invariant violated, expected ',
					_1: {
						ctor: '::',
						_0: msg,
						_1: {
							ctor: '::',
							_0: ' and got ',
							_1: {
								ctor: '::',
								_0: _elm_lang$core$Basics$toString(c),
								_1: {
									ctor: '::',
									_0: '/',
									_1: {
										ctor: '::',
										_0: lgot,
										_1: {
											ctor: '::',
											_0: '/',
											_1: {
												ctor: '::',
												_0: rgot,
												_1: {
													ctor: '::',
													_0: '\nPlease report this bug to <https://github.com/elm-lang/core/issues>',
													_1: {ctor: '[]'}
												}
											}
										}
									}
								}
							}
						}
					}
				}));
	});
var _elm_lang$core$Dict$isBBlack = function (dict) {
	var _p13 = dict;
	_v14_2:
	do {
		if (_p13.ctor === 'RBNode_elm_builtin') {
			if (_p13._0.ctor === 'BBlack') {
				return true;
			} else {
				break _v14_2;
			}
		} else {
			if (_p13._0.ctor === 'LBBlack') {
				return true;
			} else {
				break _v14_2;
			}
		}
	} while(false);
	return false;
};
var _elm_lang$core$Dict$sizeHelp = F2(
	function (n, dict) {
		sizeHelp:
		while (true) {
			var _p14 = dict;
			if (_p14.ctor === 'RBEmpty_elm_builtin') {
				return n;
			} else {
				var _v16 = A2(_elm_lang$core$Dict$sizeHelp, n + 1, _p14._4),
					_v17 = _p14._3;
				n = _v16;
				dict = _v17;
				continue sizeHelp;
			}
		}
	});
var _elm_lang$core$Dict$size = function (dict) {
	return A2(_elm_lang$core$Dict$sizeHelp, 0, dict);
};
var _elm_lang$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			var _p15 = dict;
			if (_p15.ctor === 'RBEmpty_elm_builtin') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				var _p16 = A2(_elm_lang$core$Basics$compare, targetKey, _p15._1);
				switch (_p16.ctor) {
					case 'LT':
						var _v20 = targetKey,
							_v21 = _p15._3;
						targetKey = _v20;
						dict = _v21;
						continue get;
					case 'EQ':
						return _elm_lang$core$Maybe$Just(_p15._2);
					default:
						var _v22 = targetKey,
							_v23 = _p15._4;
						targetKey = _v22;
						dict = _v23;
						continue get;
				}
			}
		}
	});
var _elm_lang$core$Dict$member = F2(
	function (key, dict) {
		var _p17 = A2(_elm_lang$core$Dict$get, key, dict);
		if (_p17.ctor === 'Just') {
			return true;
		} else {
			return false;
		}
	});
var _elm_lang$core$Dict$maxWithDefault = F3(
	function (k, v, r) {
		maxWithDefault:
		while (true) {
			var _p18 = r;
			if (_p18.ctor === 'RBEmpty_elm_builtin') {
				return {ctor: '_Tuple2', _0: k, _1: v};
			} else {
				var _v26 = _p18._1,
					_v27 = _p18._2,
					_v28 = _p18._4;
				k = _v26;
				v = _v27;
				r = _v28;
				continue maxWithDefault;
			}
		}
	});
var _elm_lang$core$Dict$NBlack = {ctor: 'NBlack'};
var _elm_lang$core$Dict$BBlack = {ctor: 'BBlack'};
var _elm_lang$core$Dict$Black = {ctor: 'Black'};
var _elm_lang$core$Dict$blackish = function (t) {
	var _p19 = t;
	if (_p19.ctor === 'RBNode_elm_builtin') {
		var _p20 = _p19._0;
		return _elm_lang$core$Native_Utils.eq(_p20, _elm_lang$core$Dict$Black) || _elm_lang$core$Native_Utils.eq(_p20, _elm_lang$core$Dict$BBlack);
	} else {
		return true;
	}
};
var _elm_lang$core$Dict$Red = {ctor: 'Red'};
var _elm_lang$core$Dict$moreBlack = function (color) {
	var _p21 = color;
	switch (_p21.ctor) {
		case 'Black':
			return _elm_lang$core$Dict$BBlack;
		case 'Red':
			return _elm_lang$core$Dict$Black;
		case 'NBlack':
			return _elm_lang$core$Dict$Red;
		default:
			return _elm_lang$core$Native_Debug.crash('Can\'t make a double black node more black!');
	}
};
var _elm_lang$core$Dict$lessBlack = function (color) {
	var _p22 = color;
	switch (_p22.ctor) {
		case 'BBlack':
			return _elm_lang$core$Dict$Black;
		case 'Black':
			return _elm_lang$core$Dict$Red;
		case 'Red':
			return _elm_lang$core$Dict$NBlack;
		default:
			return _elm_lang$core$Native_Debug.crash('Can\'t make a negative black node less black!');
	}
};
var _elm_lang$core$Dict$LBBlack = {ctor: 'LBBlack'};
var _elm_lang$core$Dict$LBlack = {ctor: 'LBlack'};
var _elm_lang$core$Dict$RBEmpty_elm_builtin = function (a) {
	return {ctor: 'RBEmpty_elm_builtin', _0: a};
};
var _elm_lang$core$Dict$empty = _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
var _elm_lang$core$Dict$isEmpty = function (dict) {
	return _elm_lang$core$Native_Utils.eq(dict, _elm_lang$core$Dict$empty);
};
var _elm_lang$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {ctor: 'RBNode_elm_builtin', _0: a, _1: b, _2: c, _3: d, _4: e};
	});
var _elm_lang$core$Dict$ensureBlackRoot = function (dict) {
	var _p23 = dict;
	if ((_p23.ctor === 'RBNode_elm_builtin') && (_p23._0.ctor === 'Red')) {
		return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p23._1, _p23._2, _p23._3, _p23._4);
	} else {
		return dict;
	}
};
var _elm_lang$core$Dict$lessBlackTree = function (dict) {
	var _p24 = dict;
	if (_p24.ctor === 'RBNode_elm_builtin') {
		return A5(
			_elm_lang$core$Dict$RBNode_elm_builtin,
			_elm_lang$core$Dict$lessBlack(_p24._0),
			_p24._1,
			_p24._2,
			_p24._3,
			_p24._4);
	} else {
		return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
	}
};
var _elm_lang$core$Dict$balancedTree = function (col) {
	return function (xk) {
		return function (xv) {
			return function (yk) {
				return function (yv) {
					return function (zk) {
						return function (zv) {
							return function (a) {
								return function (b) {
									return function (c) {
										return function (d) {
											return A5(
												_elm_lang$core$Dict$RBNode_elm_builtin,
												_elm_lang$core$Dict$lessBlack(col),
												yk,
												yv,
												A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, xk, xv, a, b),
												A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, zk, zv, c, d));
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var _elm_lang$core$Dict$blacken = function (t) {
	var _p25 = t;
	if (_p25.ctor === 'RBEmpty_elm_builtin') {
		return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
	} else {
		return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p25._1, _p25._2, _p25._3, _p25._4);
	}
};
var _elm_lang$core$Dict$redden = function (t) {
	var _p26 = t;
	if (_p26.ctor === 'RBEmpty_elm_builtin') {
		return _elm_lang$core$Native_Debug.crash('can\'t make a Leaf red');
	} else {
		return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Red, _p26._1, _p26._2, _p26._3, _p26._4);
	}
};
var _elm_lang$core$Dict$balanceHelp = function (tree) {
	var _p27 = tree;
	_v36_6:
	do {
		_v36_5:
		do {
			_v36_4:
			do {
				_v36_3:
				do {
					_v36_2:
					do {
						_v36_1:
						do {
							_v36_0:
							do {
								if (_p27.ctor === 'RBNode_elm_builtin') {
									if (_p27._3.ctor === 'RBNode_elm_builtin') {
										if (_p27._4.ctor === 'RBNode_elm_builtin') {
											switch (_p27._3._0.ctor) {
												case 'Red':
													switch (_p27._4._0.ctor) {
														case 'Red':
															if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
																break _v36_0;
															} else {
																if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
																	break _v36_1;
																} else {
																	if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
																		break _v36_2;
																	} else {
																		if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
																			break _v36_3;
																		} else {
																			break _v36_6;
																		}
																	}
																}
															}
														case 'NBlack':
															if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
																break _v36_0;
															} else {
																if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
																	break _v36_1;
																} else {
																	if (((((_p27._0.ctor === 'BBlack') && (_p27._4._3.ctor === 'RBNode_elm_builtin')) && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
																		break _v36_4;
																	} else {
																		break _v36_6;
																	}
																}
															}
														default:
															if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
																break _v36_0;
															} else {
																if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
																	break _v36_1;
																} else {
																	break _v36_6;
																}
															}
													}
												case 'NBlack':
													switch (_p27._4._0.ctor) {
														case 'Red':
															if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
																break _v36_2;
															} else {
																if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
																	break _v36_3;
																} else {
																	if (((((_p27._0.ctor === 'BBlack') && (_p27._3._3.ctor === 'RBNode_elm_builtin')) && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
																		break _v36_5;
																	} else {
																		break _v36_6;
																	}
																}
															}
														case 'NBlack':
															if (_p27._0.ctor === 'BBlack') {
																if ((((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
																	break _v36_4;
																} else {
																	if ((((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
																		break _v36_5;
																	} else {
																		break _v36_6;
																	}
																}
															} else {
																break _v36_6;
															}
														default:
															if (((((_p27._0.ctor === 'BBlack') && (_p27._3._3.ctor === 'RBNode_elm_builtin')) && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
																break _v36_5;
															} else {
																break _v36_6;
															}
													}
												default:
													switch (_p27._4._0.ctor) {
														case 'Red':
															if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
																break _v36_2;
															} else {
																if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
																	break _v36_3;
																} else {
																	break _v36_6;
																}
															}
														case 'NBlack':
															if (((((_p27._0.ctor === 'BBlack') && (_p27._4._3.ctor === 'RBNode_elm_builtin')) && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
																break _v36_4;
															} else {
																break _v36_6;
															}
														default:
															break _v36_6;
													}
											}
										} else {
											switch (_p27._3._0.ctor) {
												case 'Red':
													if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
														break _v36_0;
													} else {
														if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
															break _v36_1;
														} else {
															break _v36_6;
														}
													}
												case 'NBlack':
													if (((((_p27._0.ctor === 'BBlack') && (_p27._3._3.ctor === 'RBNode_elm_builtin')) && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
														break _v36_5;
													} else {
														break _v36_6;
													}
												default:
													break _v36_6;
											}
										}
									} else {
										if (_p27._4.ctor === 'RBNode_elm_builtin') {
											switch (_p27._4._0.ctor) {
												case 'Red':
													if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
														break _v36_2;
													} else {
														if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
															break _v36_3;
														} else {
															break _v36_6;
														}
													}
												case 'NBlack':
													if (((((_p27._0.ctor === 'BBlack') && (_p27._4._3.ctor === 'RBNode_elm_builtin')) && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
														break _v36_4;
													} else {
														break _v36_6;
													}
												default:
													break _v36_6;
											}
										} else {
											break _v36_6;
										}
									}
								} else {
									break _v36_6;
								}
							} while(false);
							return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._3._3._1)(_p27._3._3._2)(_p27._3._1)(_p27._3._2)(_p27._1)(_p27._2)(_p27._3._3._3)(_p27._3._3._4)(_p27._3._4)(_p27._4);
						} while(false);
						return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._3._1)(_p27._3._2)(_p27._3._4._1)(_p27._3._4._2)(_p27._1)(_p27._2)(_p27._3._3)(_p27._3._4._3)(_p27._3._4._4)(_p27._4);
					} while(false);
					return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._1)(_p27._2)(_p27._4._3._1)(_p27._4._3._2)(_p27._4._1)(_p27._4._2)(_p27._3)(_p27._4._3._3)(_p27._4._3._4)(_p27._4._4);
				} while(false);
				return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._1)(_p27._2)(_p27._4._1)(_p27._4._2)(_p27._4._4._1)(_p27._4._4._2)(_p27._3)(_p27._4._3)(_p27._4._4._3)(_p27._4._4._4);
			} while(false);
			return A5(
				_elm_lang$core$Dict$RBNode_elm_builtin,
				_elm_lang$core$Dict$Black,
				_p27._4._3._1,
				_p27._4._3._2,
				A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p27._1, _p27._2, _p27._3, _p27._4._3._3),
				A5(
					_elm_lang$core$Dict$balance,
					_elm_lang$core$Dict$Black,
					_p27._4._1,
					_p27._4._2,
					_p27._4._3._4,
					_elm_lang$core$Dict$redden(_p27._4._4)));
		} while(false);
		return A5(
			_elm_lang$core$Dict$RBNode_elm_builtin,
			_elm_lang$core$Dict$Black,
			_p27._3._4._1,
			_p27._3._4._2,
			A5(
				_elm_lang$core$Dict$balance,
				_elm_lang$core$Dict$Black,
				_p27._3._1,
				_p27._3._2,
				_elm_lang$core$Dict$redden(_p27._3._3),
				_p27._3._4._3),
			A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p27._1, _p27._2, _p27._3._4._4, _p27._4));
	} while(false);
	return tree;
};
var _elm_lang$core$Dict$balance = F5(
	function (c, k, v, l, r) {
		var tree = A5(_elm_lang$core$Dict$RBNode_elm_builtin, c, k, v, l, r);
		return _elm_lang$core$Dict$blackish(tree) ? _elm_lang$core$Dict$balanceHelp(tree) : tree;
	});
var _elm_lang$core$Dict$bubble = F5(
	function (c, k, v, l, r) {
		return (_elm_lang$core$Dict$isBBlack(l) || _elm_lang$core$Dict$isBBlack(r)) ? A5(
			_elm_lang$core$Dict$balance,
			_elm_lang$core$Dict$moreBlack(c),
			k,
			v,
			_elm_lang$core$Dict$lessBlackTree(l),
			_elm_lang$core$Dict$lessBlackTree(r)) : A5(_elm_lang$core$Dict$RBNode_elm_builtin, c, k, v, l, r);
	});
var _elm_lang$core$Dict$removeMax = F5(
	function (c, k, v, l, r) {
		var _p28 = r;
		if (_p28.ctor === 'RBEmpty_elm_builtin') {
			return A3(_elm_lang$core$Dict$rem, c, l, r);
		} else {
			return A5(
				_elm_lang$core$Dict$bubble,
				c,
				k,
				v,
				l,
				A5(_elm_lang$core$Dict$removeMax, _p28._0, _p28._1, _p28._2, _p28._3, _p28._4));
		}
	});
var _elm_lang$core$Dict$rem = F3(
	function (color, left, right) {
		var _p29 = {ctor: '_Tuple2', _0: left, _1: right};
		if (_p29._0.ctor === 'RBEmpty_elm_builtin') {
			if (_p29._1.ctor === 'RBEmpty_elm_builtin') {
				var _p30 = color;
				switch (_p30.ctor) {
					case 'Red':
						return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
					case 'Black':
						return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBBlack);
					default:
						return _elm_lang$core$Native_Debug.crash('cannot have bblack or nblack nodes at this point');
				}
			} else {
				var _p33 = _p29._1._0;
				var _p32 = _p29._0._0;
				var _p31 = {ctor: '_Tuple3', _0: color, _1: _p32, _2: _p33};
				if ((((_p31.ctor === '_Tuple3') && (_p31._0.ctor === 'Black')) && (_p31._1.ctor === 'LBlack')) && (_p31._2.ctor === 'Red')) {
					return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p29._1._1, _p29._1._2, _p29._1._3, _p29._1._4);
				} else {
					return A4(
						_elm_lang$core$Dict$reportRemBug,
						'Black/LBlack/Red',
						color,
						_elm_lang$core$Basics$toString(_p32),
						_elm_lang$core$Basics$toString(_p33));
				}
			}
		} else {
			if (_p29._1.ctor === 'RBEmpty_elm_builtin') {
				var _p36 = _p29._1._0;
				var _p35 = _p29._0._0;
				var _p34 = {ctor: '_Tuple3', _0: color, _1: _p35, _2: _p36};
				if ((((_p34.ctor === '_Tuple3') && (_p34._0.ctor === 'Black')) && (_p34._1.ctor === 'Red')) && (_p34._2.ctor === 'LBlack')) {
					return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p29._0._1, _p29._0._2, _p29._0._3, _p29._0._4);
				} else {
					return A4(
						_elm_lang$core$Dict$reportRemBug,
						'Black/Red/LBlack',
						color,
						_elm_lang$core$Basics$toString(_p35),
						_elm_lang$core$Basics$toString(_p36));
				}
			} else {
				var _p40 = _p29._0._2;
				var _p39 = _p29._0._4;
				var _p38 = _p29._0._1;
				var newLeft = A5(_elm_lang$core$Dict$removeMax, _p29._0._0, _p38, _p40, _p29._0._3, _p39);
				var _p37 = A3(_elm_lang$core$Dict$maxWithDefault, _p38, _p40, _p39);
				var k = _p37._0;
				var v = _p37._1;
				return A5(_elm_lang$core$Dict$bubble, color, k, v, newLeft, right);
			}
		}
	});
var _elm_lang$core$Dict$map = F2(
	function (f, dict) {
		var _p41 = dict;
		if (_p41.ctor === 'RBEmpty_elm_builtin') {
			return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
		} else {
			var _p42 = _p41._1;
			return A5(
				_elm_lang$core$Dict$RBNode_elm_builtin,
				_p41._0,
				_p42,
				A2(f, _p42, _p41._2),
				A2(_elm_lang$core$Dict$map, f, _p41._3),
				A2(_elm_lang$core$Dict$map, f, _p41._4));
		}
	});
var _elm_lang$core$Dict$Same = {ctor: 'Same'};
var _elm_lang$core$Dict$Remove = {ctor: 'Remove'};
var _elm_lang$core$Dict$Insert = {ctor: 'Insert'};
var _elm_lang$core$Dict$update = F3(
	function (k, alter, dict) {
		var up = function (dict) {
			var _p43 = dict;
			if (_p43.ctor === 'RBEmpty_elm_builtin') {
				var _p44 = alter(_elm_lang$core$Maybe$Nothing);
				if (_p44.ctor === 'Nothing') {
					return {ctor: '_Tuple2', _0: _elm_lang$core$Dict$Same, _1: _elm_lang$core$Dict$empty};
				} else {
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Dict$Insert,
						_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Red, k, _p44._0, _elm_lang$core$Dict$empty, _elm_lang$core$Dict$empty)
					};
				}
			} else {
				var _p55 = _p43._2;
				var _p54 = _p43._4;
				var _p53 = _p43._3;
				var _p52 = _p43._1;
				var _p51 = _p43._0;
				var _p45 = A2(_elm_lang$core$Basics$compare, k, _p52);
				switch (_p45.ctor) {
					case 'EQ':
						var _p46 = alter(
							_elm_lang$core$Maybe$Just(_p55));
						if (_p46.ctor === 'Nothing') {
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Dict$Remove,
								_1: A3(_elm_lang$core$Dict$rem, _p51, _p53, _p54)
							};
						} else {
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Dict$Same,
								_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _p51, _p52, _p46._0, _p53, _p54)
							};
						}
					case 'LT':
						var _p47 = up(_p53);
						var flag = _p47._0;
						var newLeft = _p47._1;
						var _p48 = flag;
						switch (_p48.ctor) {
							case 'Same':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Same,
									_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _p51, _p52, _p55, newLeft, _p54)
								};
							case 'Insert':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Insert,
									_1: A5(_elm_lang$core$Dict$balance, _p51, _p52, _p55, newLeft, _p54)
								};
							default:
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Remove,
									_1: A5(_elm_lang$core$Dict$bubble, _p51, _p52, _p55, newLeft, _p54)
								};
						}
					default:
						var _p49 = up(_p54);
						var flag = _p49._0;
						var newRight = _p49._1;
						var _p50 = flag;
						switch (_p50.ctor) {
							case 'Same':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Same,
									_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _p51, _p52, _p55, _p53, newRight)
								};
							case 'Insert':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Insert,
									_1: A5(_elm_lang$core$Dict$balance, _p51, _p52, _p55, _p53, newRight)
								};
							default:
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Remove,
									_1: A5(_elm_lang$core$Dict$bubble, _p51, _p52, _p55, _p53, newRight)
								};
						}
				}
			}
		};
		var _p56 = up(dict);
		var flag = _p56._0;
		var updatedDict = _p56._1;
		var _p57 = flag;
		switch (_p57.ctor) {
			case 'Same':
				return updatedDict;
			case 'Insert':
				return _elm_lang$core$Dict$ensureBlackRoot(updatedDict);
			default:
				return _elm_lang$core$Dict$blacken(updatedDict);
		}
	});
var _elm_lang$core$Dict$insert = F3(
	function (key, value, dict) {
		return A3(
			_elm_lang$core$Dict$update,
			key,
			_elm_lang$core$Basics$always(
				_elm_lang$core$Maybe$Just(value)),
			dict);
	});
var _elm_lang$core$Dict$singleton = F2(
	function (key, value) {
		return A3(_elm_lang$core$Dict$insert, key, value, _elm_lang$core$Dict$empty);
	});
var _elm_lang$core$Dict$union = F2(
	function (t1, t2) {
		return A3(_elm_lang$core$Dict$foldl, _elm_lang$core$Dict$insert, t2, t1);
	});
var _elm_lang$core$Dict$filter = F2(
	function (predicate, dictionary) {
		var add = F3(
			function (key, value, dict) {
				return A2(predicate, key, value) ? A3(_elm_lang$core$Dict$insert, key, value, dict) : dict;
			});
		return A3(_elm_lang$core$Dict$foldl, add, _elm_lang$core$Dict$empty, dictionary);
	});
var _elm_lang$core$Dict$intersect = F2(
	function (t1, t2) {
		return A2(
			_elm_lang$core$Dict$filter,
			F2(
				function (k, _p58) {
					return A2(_elm_lang$core$Dict$member, k, t2);
				}),
			t1);
	});
var _elm_lang$core$Dict$partition = F2(
	function (predicate, dict) {
		var add = F3(
			function (key, value, _p59) {
				var _p60 = _p59;
				var _p62 = _p60._1;
				var _p61 = _p60._0;
				return A2(predicate, key, value) ? {
					ctor: '_Tuple2',
					_0: A3(_elm_lang$core$Dict$insert, key, value, _p61),
					_1: _p62
				} : {
					ctor: '_Tuple2',
					_0: _p61,
					_1: A3(_elm_lang$core$Dict$insert, key, value, _p62)
				};
			});
		return A3(
			_elm_lang$core$Dict$foldl,
			add,
			{ctor: '_Tuple2', _0: _elm_lang$core$Dict$empty, _1: _elm_lang$core$Dict$empty},
			dict);
	});
var _elm_lang$core$Dict$fromList = function (assocs) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (_p63, dict) {
				var _p64 = _p63;
				return A3(_elm_lang$core$Dict$insert, _p64._0, _p64._1, dict);
			}),
		_elm_lang$core$Dict$empty,
		assocs);
};
var _elm_lang$core$Dict$remove = F2(
	function (key, dict) {
		return A3(
			_elm_lang$core$Dict$update,
			key,
			_elm_lang$core$Basics$always(_elm_lang$core$Maybe$Nothing),
			dict);
	});
var _elm_lang$core$Dict$diff = F2(
	function (t1, t2) {
		return A3(
			_elm_lang$core$Dict$foldl,
			F3(
				function (k, v, t) {
					return A2(_elm_lang$core$Dict$remove, k, t);
				}),
			t1,
			t2);
	});

//import Native.Scheduler //

var _elm_lang$core$Native_Time = function() {

var now = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
{
	callback(_elm_lang$core$Native_Scheduler.succeed(Date.now()));
});

function setInterval_(interval, task)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		var id = setInterval(function() {
			_elm_lang$core$Native_Scheduler.rawSpawn(task);
		}, interval);

		return function() { clearInterval(id); };
	});
}

return {
	now: now,
	setInterval_: F2(setInterval_)
};

}();
var _elm_lang$core$Time$setInterval = _elm_lang$core$Native_Time.setInterval_;
var _elm_lang$core$Time$spawnHelp = F3(
	function (router, intervals, processes) {
		var _p0 = intervals;
		if (_p0.ctor === '[]') {
			return _elm_lang$core$Task$succeed(processes);
		} else {
			var _p1 = _p0._0;
			var spawnRest = function (id) {
				return A3(
					_elm_lang$core$Time$spawnHelp,
					router,
					_p0._1,
					A3(_elm_lang$core$Dict$insert, _p1, id, processes));
			};
			var spawnTimer = _elm_lang$core$Native_Scheduler.spawn(
				A2(
					_elm_lang$core$Time$setInterval,
					_p1,
					A2(_elm_lang$core$Platform$sendToSelf, router, _p1)));
			return A2(_elm_lang$core$Task$andThen, spawnRest, spawnTimer);
		}
	});
var _elm_lang$core$Time$addMySub = F2(
	function (_p2, state) {
		var _p3 = _p2;
		var _p6 = _p3._1;
		var _p5 = _p3._0;
		var _p4 = A2(_elm_lang$core$Dict$get, _p5, state);
		if (_p4.ctor === 'Nothing') {
			return A3(
				_elm_lang$core$Dict$insert,
				_p5,
				{
					ctor: '::',
					_0: _p6,
					_1: {ctor: '[]'}
				},
				state);
		} else {
			return A3(
				_elm_lang$core$Dict$insert,
				_p5,
				{ctor: '::', _0: _p6, _1: _p4._0},
				state);
		}
	});
var _elm_lang$core$Time$inMilliseconds = function (t) {
	return t;
};
var _elm_lang$core$Time$millisecond = 1;
var _elm_lang$core$Time$second = 1000 * _elm_lang$core$Time$millisecond;
var _elm_lang$core$Time$minute = 60 * _elm_lang$core$Time$second;
var _elm_lang$core$Time$hour = 60 * _elm_lang$core$Time$minute;
var _elm_lang$core$Time$inHours = function (t) {
	return t / _elm_lang$core$Time$hour;
};
var _elm_lang$core$Time$inMinutes = function (t) {
	return t / _elm_lang$core$Time$minute;
};
var _elm_lang$core$Time$inSeconds = function (t) {
	return t / _elm_lang$core$Time$second;
};
var _elm_lang$core$Time$now = _elm_lang$core$Native_Time.now;
var _elm_lang$core$Time$onSelfMsg = F3(
	function (router, interval, state) {
		var _p7 = A2(_elm_lang$core$Dict$get, interval, state.taggers);
		if (_p7.ctor === 'Nothing') {
			return _elm_lang$core$Task$succeed(state);
		} else {
			var tellTaggers = function (time) {
				return _elm_lang$core$Task$sequence(
					A2(
						_elm_lang$core$List$map,
						function (tagger) {
							return A2(
								_elm_lang$core$Platform$sendToApp,
								router,
								tagger(time));
						},
						_p7._0));
			};
			return A2(
				_elm_lang$core$Task$andThen,
				function (_p8) {
					return _elm_lang$core$Task$succeed(state);
				},
				A2(_elm_lang$core$Task$andThen, tellTaggers, _elm_lang$core$Time$now));
		}
	});
var _elm_lang$core$Time$subscription = _elm_lang$core$Native_Platform.leaf('Time');
var _elm_lang$core$Time$State = F2(
	function (a, b) {
		return {taggers: a, processes: b};
	});
var _elm_lang$core$Time$init = _elm_lang$core$Task$succeed(
	A2(_elm_lang$core$Time$State, _elm_lang$core$Dict$empty, _elm_lang$core$Dict$empty));
var _elm_lang$core$Time$onEffects = F3(
	function (router, subs, _p9) {
		var _p10 = _p9;
		var rightStep = F3(
			function (_p12, id, _p11) {
				var _p13 = _p11;
				return {
					ctor: '_Tuple3',
					_0: _p13._0,
					_1: _p13._1,
					_2: A2(
						_elm_lang$core$Task$andThen,
						function (_p14) {
							return _p13._2;
						},
						_elm_lang$core$Native_Scheduler.kill(id))
				};
			});
		var bothStep = F4(
			function (interval, taggers, id, _p15) {
				var _p16 = _p15;
				return {
					ctor: '_Tuple3',
					_0: _p16._0,
					_1: A3(_elm_lang$core$Dict$insert, interval, id, _p16._1),
					_2: _p16._2
				};
			});
		var leftStep = F3(
			function (interval, taggers, _p17) {
				var _p18 = _p17;
				return {
					ctor: '_Tuple3',
					_0: {ctor: '::', _0: interval, _1: _p18._0},
					_1: _p18._1,
					_2: _p18._2
				};
			});
		var newTaggers = A3(_elm_lang$core$List$foldl, _elm_lang$core$Time$addMySub, _elm_lang$core$Dict$empty, subs);
		var _p19 = A6(
			_elm_lang$core$Dict$merge,
			leftStep,
			bothStep,
			rightStep,
			newTaggers,
			_p10.processes,
			{
				ctor: '_Tuple3',
				_0: {ctor: '[]'},
				_1: _elm_lang$core$Dict$empty,
				_2: _elm_lang$core$Task$succeed(
					{ctor: '_Tuple0'})
			});
		var spawnList = _p19._0;
		var existingDict = _p19._1;
		var killTask = _p19._2;
		return A2(
			_elm_lang$core$Task$andThen,
			function (newProcesses) {
				return _elm_lang$core$Task$succeed(
					A2(_elm_lang$core$Time$State, newTaggers, newProcesses));
			},
			A2(
				_elm_lang$core$Task$andThen,
				function (_p20) {
					return A3(_elm_lang$core$Time$spawnHelp, router, spawnList, existingDict);
				},
				killTask));
	});
var _elm_lang$core$Time$Every = F2(
	function (a, b) {
		return {ctor: 'Every', _0: a, _1: b};
	});
var _elm_lang$core$Time$every = F2(
	function (interval, tagger) {
		return _elm_lang$core$Time$subscription(
			A2(_elm_lang$core$Time$Every, interval, tagger));
	});
var _elm_lang$core$Time$subMap = F2(
	function (f, _p21) {
		var _p22 = _p21;
		return A2(
			_elm_lang$core$Time$Every,
			_p22._0,
			function (_p23) {
				return f(
					_p22._1(_p23));
			});
	});
_elm_lang$core$Native_Platform.effectManagers['Time'] = {pkg: 'elm-lang/core', init: _elm_lang$core$Time$init, onEffects: _elm_lang$core$Time$onEffects, onSelfMsg: _elm_lang$core$Time$onSelfMsg, tag: 'sub', subMap: _elm_lang$core$Time$subMap};

var _elm_lang$core$Debug$crash = _elm_lang$core$Native_Debug.crash;
var _elm_lang$core$Debug$log = _elm_lang$core$Native_Debug.log;

//import Maybe, Native.Array, Native.List, Native.Utils, Result //

var _elm_lang$core$Native_Json = function() {


// CORE DECODERS

function succeed(msg)
{
	return {
		ctor: '<decoder>',
		tag: 'succeed',
		msg: msg
	};
}

function fail(msg)
{
	return {
		ctor: '<decoder>',
		tag: 'fail',
		msg: msg
	};
}

function decodePrimitive(tag)
{
	return {
		ctor: '<decoder>',
		tag: tag
	};
}

function decodeContainer(tag, decoder)
{
	return {
		ctor: '<decoder>',
		tag: tag,
		decoder: decoder
	};
}

function decodeNull(value)
{
	return {
		ctor: '<decoder>',
		tag: 'null',
		value: value
	};
}

function decodeField(field, decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'field',
		field: field,
		decoder: decoder
	};
}

function decodeIndex(index, decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'index',
		index: index,
		decoder: decoder
	};
}

function decodeKeyValuePairs(decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'key-value',
		decoder: decoder
	};
}

function mapMany(f, decoders)
{
	return {
		ctor: '<decoder>',
		tag: 'map-many',
		func: f,
		decoders: decoders
	};
}

function andThen(callback, decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'andThen',
		decoder: decoder,
		callback: callback
	};
}

function oneOf(decoders)
{
	return {
		ctor: '<decoder>',
		tag: 'oneOf',
		decoders: decoders
	};
}


// DECODING OBJECTS

function map1(f, d1)
{
	return mapMany(f, [d1]);
}

function map2(f, d1, d2)
{
	return mapMany(f, [d1, d2]);
}

function map3(f, d1, d2, d3)
{
	return mapMany(f, [d1, d2, d3]);
}

function map4(f, d1, d2, d3, d4)
{
	return mapMany(f, [d1, d2, d3, d4]);
}

function map5(f, d1, d2, d3, d4, d5)
{
	return mapMany(f, [d1, d2, d3, d4, d5]);
}

function map6(f, d1, d2, d3, d4, d5, d6)
{
	return mapMany(f, [d1, d2, d3, d4, d5, d6]);
}

function map7(f, d1, d2, d3, d4, d5, d6, d7)
{
	return mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
}

function map8(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
}


// DECODE HELPERS

function ok(value)
{
	return { tag: 'ok', value: value };
}

function badPrimitive(type, value)
{
	return { tag: 'primitive', type: type, value: value };
}

function badIndex(index, nestedProblems)
{
	return { tag: 'index', index: index, rest: nestedProblems };
}

function badField(field, nestedProblems)
{
	return { tag: 'field', field: field, rest: nestedProblems };
}

function badIndex(index, nestedProblems)
{
	return { tag: 'index', index: index, rest: nestedProblems };
}

function badOneOf(problems)
{
	return { tag: 'oneOf', problems: problems };
}

function bad(msg)
{
	return { tag: 'fail', msg: msg };
}

function badToString(problem)
{
	var context = '_';
	while (problem)
	{
		switch (problem.tag)
		{
			case 'primitive':
				return 'Expecting ' + problem.type
					+ (context === '_' ? '' : ' at ' + context)
					+ ' but instead got: ' + jsToString(problem.value);

			case 'index':
				context += '[' + problem.index + ']';
				problem = problem.rest;
				break;

			case 'field':
				context += '.' + problem.field;
				problem = problem.rest;
				break;

			case 'oneOf':
				var problems = problem.problems;
				for (var i = 0; i < problems.length; i++)
				{
					problems[i] = badToString(problems[i]);
				}
				return 'I ran into the following problems'
					+ (context === '_' ? '' : ' at ' + context)
					+ ':\n\n' + problems.join('\n');

			case 'fail':
				return 'I ran into a `fail` decoder'
					+ (context === '_' ? '' : ' at ' + context)
					+ ': ' + problem.msg;
		}
	}
}

function jsToString(value)
{
	return value === undefined
		? 'undefined'
		: JSON.stringify(value);
}


// DECODE

function runOnString(decoder, string)
{
	var json;
	try
	{
		json = JSON.parse(string);
	}
	catch (e)
	{
		return _elm_lang$core$Result$Err('Given an invalid JSON: ' + e.message);
	}
	return run(decoder, json);
}

function run(decoder, value)
{
	var result = runHelp(decoder, value);
	return (result.tag === 'ok')
		? _elm_lang$core$Result$Ok(result.value)
		: _elm_lang$core$Result$Err(badToString(result));
}

function runHelp(decoder, value)
{
	switch (decoder.tag)
	{
		case 'bool':
			return (typeof value === 'boolean')
				? ok(value)
				: badPrimitive('a Bool', value);

		case 'int':
			if (typeof value !== 'number') {
				return badPrimitive('an Int', value);
			}

			if (-2147483647 < value && value < 2147483647 && (value | 0) === value) {
				return ok(value);
			}

			if (isFinite(value) && !(value % 1)) {
				return ok(value);
			}

			return badPrimitive('an Int', value);

		case 'float':
			return (typeof value === 'number')
				? ok(value)
				: badPrimitive('a Float', value);

		case 'string':
			return (typeof value === 'string')
				? ok(value)
				: (value instanceof String)
					? ok(value + '')
					: badPrimitive('a String', value);

		case 'null':
			return (value === null)
				? ok(decoder.value)
				: badPrimitive('null', value);

		case 'value':
			return ok(value);

		case 'list':
			if (!(value instanceof Array))
			{
				return badPrimitive('a List', value);
			}

			var list = _elm_lang$core$Native_List.Nil;
			for (var i = value.length; i--; )
			{
				var result = runHelp(decoder.decoder, value[i]);
				if (result.tag !== 'ok')
				{
					return badIndex(i, result)
				}
				list = _elm_lang$core$Native_List.Cons(result.value, list);
			}
			return ok(list);

		case 'array':
			if (!(value instanceof Array))
			{
				return badPrimitive('an Array', value);
			}

			var len = value.length;
			var array = new Array(len);
			for (var i = len; i--; )
			{
				var result = runHelp(decoder.decoder, value[i]);
				if (result.tag !== 'ok')
				{
					return badIndex(i, result);
				}
				array[i] = result.value;
			}
			return ok(_elm_lang$core$Native_Array.fromJSArray(array));

		case 'maybe':
			var result = runHelp(decoder.decoder, value);
			return (result.tag === 'ok')
				? ok(_elm_lang$core$Maybe$Just(result.value))
				: ok(_elm_lang$core$Maybe$Nothing);

		case 'field':
			var field = decoder.field;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return badPrimitive('an object with a field named `' + field + '`', value);
			}

			var result = runHelp(decoder.decoder, value[field]);
			return (result.tag === 'ok') ? result : badField(field, result);

		case 'index':
			var index = decoder.index;
			if (!(value instanceof Array))
			{
				return badPrimitive('an array', value);
			}
			if (index >= value.length)
			{
				return badPrimitive('a longer array. Need index ' + index + ' but there are only ' + value.length + ' entries', value);
			}

			var result = runHelp(decoder.decoder, value[index]);
			return (result.tag === 'ok') ? result : badIndex(index, result);

		case 'key-value':
			if (typeof value !== 'object' || value === null || value instanceof Array)
			{
				return badPrimitive('an object', value);
			}

			var keyValuePairs = _elm_lang$core$Native_List.Nil;
			for (var key in value)
			{
				var result = runHelp(decoder.decoder, value[key]);
				if (result.tag !== 'ok')
				{
					return badField(key, result);
				}
				var pair = _elm_lang$core$Native_Utils.Tuple2(key, result.value);
				keyValuePairs = _elm_lang$core$Native_List.Cons(pair, keyValuePairs);
			}
			return ok(keyValuePairs);

		case 'map-many':
			var answer = decoder.func;
			var decoders = decoder.decoders;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = runHelp(decoders[i], value);
				if (result.tag !== 'ok')
				{
					return result;
				}
				answer = answer(result.value);
			}
			return ok(answer);

		case 'andThen':
			var result = runHelp(decoder.decoder, value);
			return (result.tag !== 'ok')
				? result
				: runHelp(decoder.callback(result.value), value);

		case 'oneOf':
			var errors = [];
			var temp = decoder.decoders;
			while (temp.ctor !== '[]')
			{
				var result = runHelp(temp._0, value);

				if (result.tag === 'ok')
				{
					return result;
				}

				errors.push(result);

				temp = temp._1;
			}
			return badOneOf(errors);

		case 'fail':
			return bad(decoder.msg);

		case 'succeed':
			return ok(decoder.msg);
	}
}


// EQUALITY

function equality(a, b)
{
	if (a === b)
	{
		return true;
	}

	if (a.tag !== b.tag)
	{
		return false;
	}

	switch (a.tag)
	{
		case 'succeed':
		case 'fail':
			return a.msg === b.msg;

		case 'bool':
		case 'int':
		case 'float':
		case 'string':
		case 'value':
			return true;

		case 'null':
			return a.value === b.value;

		case 'list':
		case 'array':
		case 'maybe':
		case 'key-value':
			return equality(a.decoder, b.decoder);

		case 'field':
			return a.field === b.field && equality(a.decoder, b.decoder);

		case 'index':
			return a.index === b.index && equality(a.decoder, b.decoder);

		case 'map-many':
			if (a.func !== b.func)
			{
				return false;
			}
			return listEquality(a.decoders, b.decoders);

		case 'andThen':
			return a.callback === b.callback && equality(a.decoder, b.decoder);

		case 'oneOf':
			return listEquality(a.decoders, b.decoders);
	}
}

function listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

function encode(indentLevel, value)
{
	return JSON.stringify(value, null, indentLevel);
}

function identity(value)
{
	return value;
}

function encodeObject(keyValuePairs)
{
	var obj = {};
	while (keyValuePairs.ctor !== '[]')
	{
		var pair = keyValuePairs._0;
		obj[pair._0] = pair._1;
		keyValuePairs = keyValuePairs._1;
	}
	return obj;
}

return {
	encode: F2(encode),
	runOnString: F2(runOnString),
	run: F2(run),

	decodeNull: decodeNull,
	decodePrimitive: decodePrimitive,
	decodeContainer: F2(decodeContainer),

	decodeField: F2(decodeField),
	decodeIndex: F2(decodeIndex),

	map1: F2(map1),
	map2: F3(map2),
	map3: F4(map3),
	map4: F5(map4),
	map5: F6(map5),
	map6: F7(map6),
	map7: F8(map7),
	map8: F9(map8),
	decodeKeyValuePairs: decodeKeyValuePairs,

	andThen: F2(andThen),
	fail: fail,
	succeed: succeed,
	oneOf: oneOf,

	identity: identity,
	encodeNull: null,
	encodeArray: _elm_lang$core$Native_Array.toJSArray,
	encodeList: _elm_lang$core$Native_List.toArray,
	encodeObject: encodeObject,

	equality: equality
};

}();

var _elm_lang$core$Json_Encode$list = _elm_lang$core$Native_Json.encodeList;
var _elm_lang$core$Json_Encode$array = _elm_lang$core$Native_Json.encodeArray;
var _elm_lang$core$Json_Encode$object = _elm_lang$core$Native_Json.encodeObject;
var _elm_lang$core$Json_Encode$null = _elm_lang$core$Native_Json.encodeNull;
var _elm_lang$core$Json_Encode$bool = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$float = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$int = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$string = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$encode = _elm_lang$core$Native_Json.encode;
var _elm_lang$core$Json_Encode$Value = {ctor: 'Value'};

var _elm_lang$core$Json_Decode$null = _elm_lang$core$Native_Json.decodeNull;
var _elm_lang$core$Json_Decode$value = _elm_lang$core$Native_Json.decodePrimitive('value');
var _elm_lang$core$Json_Decode$andThen = _elm_lang$core$Native_Json.andThen;
var _elm_lang$core$Json_Decode$fail = _elm_lang$core$Native_Json.fail;
var _elm_lang$core$Json_Decode$succeed = _elm_lang$core$Native_Json.succeed;
var _elm_lang$core$Json_Decode$lazy = function (thunk) {
	return A2(
		_elm_lang$core$Json_Decode$andThen,
		thunk,
		_elm_lang$core$Json_Decode$succeed(
			{ctor: '_Tuple0'}));
};
var _elm_lang$core$Json_Decode$decodeValue = _elm_lang$core$Native_Json.run;
var _elm_lang$core$Json_Decode$decodeString = _elm_lang$core$Native_Json.runOnString;
var _elm_lang$core$Json_Decode$map8 = _elm_lang$core$Native_Json.map8;
var _elm_lang$core$Json_Decode$map7 = _elm_lang$core$Native_Json.map7;
var _elm_lang$core$Json_Decode$map6 = _elm_lang$core$Native_Json.map6;
var _elm_lang$core$Json_Decode$map5 = _elm_lang$core$Native_Json.map5;
var _elm_lang$core$Json_Decode$map4 = _elm_lang$core$Native_Json.map4;
var _elm_lang$core$Json_Decode$map3 = _elm_lang$core$Native_Json.map3;
var _elm_lang$core$Json_Decode$map2 = _elm_lang$core$Native_Json.map2;
var _elm_lang$core$Json_Decode$map = _elm_lang$core$Native_Json.map1;
var _elm_lang$core$Json_Decode$oneOf = _elm_lang$core$Native_Json.oneOf;
var _elm_lang$core$Json_Decode$maybe = function (decoder) {
	return A2(_elm_lang$core$Native_Json.decodeContainer, 'maybe', decoder);
};
var _elm_lang$core$Json_Decode$index = _elm_lang$core$Native_Json.decodeIndex;
var _elm_lang$core$Json_Decode$field = _elm_lang$core$Native_Json.decodeField;
var _elm_lang$core$Json_Decode$at = F2(
	function (fields, decoder) {
		return A3(_elm_lang$core$List$foldr, _elm_lang$core$Json_Decode$field, decoder, fields);
	});
var _elm_lang$core$Json_Decode$keyValuePairs = _elm_lang$core$Native_Json.decodeKeyValuePairs;
var _elm_lang$core$Json_Decode$dict = function (decoder) {
	return A2(
		_elm_lang$core$Json_Decode$map,
		_elm_lang$core$Dict$fromList,
		_elm_lang$core$Json_Decode$keyValuePairs(decoder));
};
var _elm_lang$core$Json_Decode$array = function (decoder) {
	return A2(_elm_lang$core$Native_Json.decodeContainer, 'array', decoder);
};
var _elm_lang$core$Json_Decode$list = function (decoder) {
	return A2(_elm_lang$core$Native_Json.decodeContainer, 'list', decoder);
};
var _elm_lang$core$Json_Decode$nullable = function (decoder) {
	return _elm_lang$core$Json_Decode$oneOf(
		{
			ctor: '::',
			_0: _elm_lang$core$Json_Decode$null(_elm_lang$core$Maybe$Nothing),
			_1: {
				ctor: '::',
				_0: A2(_elm_lang$core$Json_Decode$map, _elm_lang$core$Maybe$Just, decoder),
				_1: {ctor: '[]'}
			}
		});
};
var _elm_lang$core$Json_Decode$float = _elm_lang$core$Native_Json.decodePrimitive('float');
var _elm_lang$core$Json_Decode$int = _elm_lang$core$Native_Json.decodePrimitive('int');
var _elm_lang$core$Json_Decode$bool = _elm_lang$core$Native_Json.decodePrimitive('bool');
var _elm_lang$core$Json_Decode$string = _elm_lang$core$Native_Json.decodePrimitive('string');
var _elm_lang$core$Json_Decode$Decoder = {ctor: 'Decoder'};

//import Maybe, Native.List //

var _elm_lang$core$Native_Regex = function() {

function escape(str)
{
	return str.replace(/[-\/\\^$*+?.()|[\]{}]/g, '\\$&');
}
function caseInsensitive(re)
{
	return new RegExp(re.source, 'gi');
}
function regex(raw)
{
	return new RegExp(raw, 'g');
}

function contains(re, string)
{
	return string.match(re) !== null;
}

function find(n, re, str)
{
	n = n.ctor === 'All' ? Infinity : n._0;
	var out = [];
	var number = 0;
	var string = str;
	var lastIndex = re.lastIndex;
	var prevLastIndex = -1;
	var result;
	while (number++ < n && (result = re.exec(string)))
	{
		if (prevLastIndex === re.lastIndex) break;
		var i = result.length - 1;
		var subs = new Array(i);
		while (i > 0)
		{
			var submatch = result[i];
			subs[--i] = submatch === undefined
				? _elm_lang$core$Maybe$Nothing
				: _elm_lang$core$Maybe$Just(submatch);
		}
		out.push({
			match: result[0],
			submatches: _elm_lang$core$Native_List.fromArray(subs),
			index: result.index,
			number: number
		});
		prevLastIndex = re.lastIndex;
	}
	re.lastIndex = lastIndex;
	return _elm_lang$core$Native_List.fromArray(out);
}

function replace(n, re, replacer, string)
{
	n = n.ctor === 'All' ? Infinity : n._0;
	var count = 0;
	function jsReplacer(match)
	{
		if (count++ >= n)
		{
			return match;
		}
		var i = arguments.length - 3;
		var submatches = new Array(i);
		while (i > 0)
		{
			var submatch = arguments[i];
			submatches[--i] = submatch === undefined
				? _elm_lang$core$Maybe$Nothing
				: _elm_lang$core$Maybe$Just(submatch);
		}
		return replacer({
			match: match,
			submatches: _elm_lang$core$Native_List.fromArray(submatches),
			index: arguments[arguments.length - 2],
			number: count
		});
	}
	return string.replace(re, jsReplacer);
}

function split(n, re, str)
{
	n = n.ctor === 'All' ? Infinity : n._0;
	if (n === Infinity)
	{
		return _elm_lang$core$Native_List.fromArray(str.split(re));
	}
	var string = str;
	var result;
	var out = [];
	var start = re.lastIndex;
	var restoreLastIndex = re.lastIndex;
	while (n--)
	{
		if (!(result = re.exec(string))) break;
		out.push(string.slice(start, result.index));
		start = re.lastIndex;
	}
	out.push(string.slice(start));
	re.lastIndex = restoreLastIndex;
	return _elm_lang$core$Native_List.fromArray(out);
}

return {
	regex: regex,
	caseInsensitive: caseInsensitive,
	escape: escape,

	contains: F2(contains),
	find: F3(find),
	replace: F4(replace),
	split: F3(split)
};

}();

var _elm_lang$core$Process$kill = _elm_lang$core$Native_Scheduler.kill;
var _elm_lang$core$Process$sleep = _elm_lang$core$Native_Scheduler.sleep;
var _elm_lang$core$Process$spawn = _elm_lang$core$Native_Scheduler.spawn;

var _elm_lang$core$Tuple$mapSecond = F2(
	function (func, _p0) {
		var _p1 = _p0;
		return {
			ctor: '_Tuple2',
			_0: _p1._0,
			_1: func(_p1._1)
		};
	});
var _elm_lang$core$Tuple$mapFirst = F2(
	function (func, _p2) {
		var _p3 = _p2;
		return {
			ctor: '_Tuple2',
			_0: func(_p3._0),
			_1: _p3._1
		};
	});
var _elm_lang$core$Tuple$second = function (_p4) {
	var _p5 = _p4;
	return _p5._1;
};
var _elm_lang$core$Tuple$first = function (_p6) {
	var _p7 = _p6;
	return _p7._0;
};

var _elm_lang$core$Regex$split = _elm_lang$core$Native_Regex.split;
var _elm_lang$core$Regex$replace = _elm_lang$core$Native_Regex.replace;
var _elm_lang$core$Regex$find = _elm_lang$core$Native_Regex.find;
var _elm_lang$core$Regex$contains = _elm_lang$core$Native_Regex.contains;
var _elm_lang$core$Regex$caseInsensitive = _elm_lang$core$Native_Regex.caseInsensitive;
var _elm_lang$core$Regex$regex = _elm_lang$core$Native_Regex.regex;
var _elm_lang$core$Regex$escape = _elm_lang$core$Native_Regex.escape;
var _elm_lang$core$Regex$Match = F4(
	function (a, b, c, d) {
		return {match: a, submatches: b, index: c, number: d};
	});
var _elm_lang$core$Regex$Regex = {ctor: 'Regex'};
var _elm_lang$core$Regex$AtMost = function (a) {
	return {ctor: 'AtMost', _0: a};
};
var _elm_lang$core$Regex$All = {ctor: 'All'};

var _elm_lang$core$Set$foldr = F3(
	function (f, b, _p0) {
		var _p1 = _p0;
		return A3(
			_elm_lang$core$Dict$foldr,
			F3(
				function (k, _p2, b) {
					return A2(f, k, b);
				}),
			b,
			_p1._0);
	});
var _elm_lang$core$Set$foldl = F3(
	function (f, b, _p3) {
		var _p4 = _p3;
		return A3(
			_elm_lang$core$Dict$foldl,
			F3(
				function (k, _p5, b) {
					return A2(f, k, b);
				}),
			b,
			_p4._0);
	});
var _elm_lang$core$Set$toList = function (_p6) {
	var _p7 = _p6;
	return _elm_lang$core$Dict$keys(_p7._0);
};
var _elm_lang$core$Set$size = function (_p8) {
	var _p9 = _p8;
	return _elm_lang$core$Dict$size(_p9._0);
};
var _elm_lang$core$Set$member = F2(
	function (k, _p10) {
		var _p11 = _p10;
		return A2(_elm_lang$core$Dict$member, k, _p11._0);
	});
var _elm_lang$core$Set$isEmpty = function (_p12) {
	var _p13 = _p12;
	return _elm_lang$core$Dict$isEmpty(_p13._0);
};
var _elm_lang$core$Set$Set_elm_builtin = function (a) {
	return {ctor: 'Set_elm_builtin', _0: a};
};
var _elm_lang$core$Set$empty = _elm_lang$core$Set$Set_elm_builtin(_elm_lang$core$Dict$empty);
var _elm_lang$core$Set$singleton = function (k) {
	return _elm_lang$core$Set$Set_elm_builtin(
		A2(
			_elm_lang$core$Dict$singleton,
			k,
			{ctor: '_Tuple0'}));
};
var _elm_lang$core$Set$insert = F2(
	function (k, _p14) {
		var _p15 = _p14;
		return _elm_lang$core$Set$Set_elm_builtin(
			A3(
				_elm_lang$core$Dict$insert,
				k,
				{ctor: '_Tuple0'},
				_p15._0));
	});
var _elm_lang$core$Set$fromList = function (xs) {
	return A3(_elm_lang$core$List$foldl, _elm_lang$core$Set$insert, _elm_lang$core$Set$empty, xs);
};
var _elm_lang$core$Set$map = F2(
	function (f, s) {
		return _elm_lang$core$Set$fromList(
			A2(
				_elm_lang$core$List$map,
				f,
				_elm_lang$core$Set$toList(s)));
	});
var _elm_lang$core$Set$remove = F2(
	function (k, _p16) {
		var _p17 = _p16;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(_elm_lang$core$Dict$remove, k, _p17._0));
	});
var _elm_lang$core$Set$union = F2(
	function (_p19, _p18) {
		var _p20 = _p19;
		var _p21 = _p18;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(_elm_lang$core$Dict$union, _p20._0, _p21._0));
	});
var _elm_lang$core$Set$intersect = F2(
	function (_p23, _p22) {
		var _p24 = _p23;
		var _p25 = _p22;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(_elm_lang$core$Dict$intersect, _p24._0, _p25._0));
	});
var _elm_lang$core$Set$diff = F2(
	function (_p27, _p26) {
		var _p28 = _p27;
		var _p29 = _p26;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(_elm_lang$core$Dict$diff, _p28._0, _p29._0));
	});
var _elm_lang$core$Set$filter = F2(
	function (p, _p30) {
		var _p31 = _p30;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(
				_elm_lang$core$Dict$filter,
				F2(
					function (k, _p32) {
						return p(k);
					}),
				_p31._0));
	});
var _elm_lang$core$Set$partition = F2(
	function (p, _p33) {
		var _p34 = _p33;
		var _p35 = A2(
			_elm_lang$core$Dict$partition,
			F2(
				function (k, _p36) {
					return p(k);
				}),
			_p34._0);
		var p1 = _p35._0;
		var p2 = _p35._1;
		return {
			ctor: '_Tuple2',
			_0: _elm_lang$core$Set$Set_elm_builtin(p1),
			_1: _elm_lang$core$Set$Set_elm_builtin(p2)
		};
	});

var _elm_lang$dom$Native_Dom = function() {

var fakeNode = {
	addEventListener: function() {},
	removeEventListener: function() {}
};

var onDocument = on(typeof document !== 'undefined' ? document : fakeNode);
var onWindow = on(typeof window !== 'undefined' ? window : fakeNode);

function on(node)
{
	return function(eventName, decoder, toTask)
	{
		return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {

			function performTask(event)
			{
				var result = A2(_elm_lang$core$Json_Decode$decodeValue, decoder, event);
				if (result.ctor === 'Ok')
				{
					_elm_lang$core$Native_Scheduler.rawSpawn(toTask(result._0));
				}
			}

			node.addEventListener(eventName, performTask);

			return function()
			{
				node.removeEventListener(eventName, performTask);
			};
		});
	};
}

var rAF = typeof requestAnimationFrame !== 'undefined'
	? requestAnimationFrame
	: function(callback) { callback(); };

function withNode(id, doStuff)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		rAF(function()
		{
			var node = document.getElementById(id);
			if (node === null)
			{
				callback(_elm_lang$core$Native_Scheduler.fail({ ctor: 'NotFound', _0: id }));
				return;
			}
			callback(_elm_lang$core$Native_Scheduler.succeed(doStuff(node)));
		});
	});
}


// FOCUS

function focus(id)
{
	return withNode(id, function(node) {
		node.focus();
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}

function blur(id)
{
	return withNode(id, function(node) {
		node.blur();
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}


// SCROLLING

function getScrollTop(id)
{
	return withNode(id, function(node) {
		return node.scrollTop;
	});
}

function setScrollTop(id, desiredScrollTop)
{
	return withNode(id, function(node) {
		node.scrollTop = desiredScrollTop;
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}

function toBottom(id)
{
	return withNode(id, function(node) {
		node.scrollTop = node.scrollHeight;
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}

function getScrollLeft(id)
{
	return withNode(id, function(node) {
		return node.scrollLeft;
	});
}

function setScrollLeft(id, desiredScrollLeft)
{
	return withNode(id, function(node) {
		node.scrollLeft = desiredScrollLeft;
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}

function toRight(id)
{
	return withNode(id, function(node) {
		node.scrollLeft = node.scrollWidth;
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}


// SIZE

function width(options, id)
{
	return withNode(id, function(node) {
		switch (options.ctor)
		{
			case 'Content':
				return node.scrollWidth;
			case 'VisibleContent':
				return node.clientWidth;
			case 'VisibleContentWithBorders':
				return node.offsetWidth;
			case 'VisibleContentWithBordersAndMargins':
				var rect = node.getBoundingClientRect();
				return rect.right - rect.left;
		}
	});
}

function height(options, id)
{
	return withNode(id, function(node) {
		switch (options.ctor)
		{
			case 'Content':
				return node.scrollHeight;
			case 'VisibleContent':
				return node.clientHeight;
			case 'VisibleContentWithBorders':
				return node.offsetHeight;
			case 'VisibleContentWithBordersAndMargins':
				var rect = node.getBoundingClientRect();
				return rect.bottom - rect.top;
		}
	});
}

return {
	onDocument: F3(onDocument),
	onWindow: F3(onWindow),

	focus: focus,
	blur: blur,

	getScrollTop: getScrollTop,
	setScrollTop: F2(setScrollTop),
	getScrollLeft: getScrollLeft,
	setScrollLeft: F2(setScrollLeft),
	toBottom: toBottom,
	toRight: toRight,

	height: F2(height),
	width: F2(width)
};

}();

var _elm_lang$dom$Dom_LowLevel$onWindow = _elm_lang$dom$Native_Dom.onWindow;
var _elm_lang$dom$Dom_LowLevel$onDocument = _elm_lang$dom$Native_Dom.onDocument;

var _elm_lang$virtual_dom$VirtualDom_Debug$wrap;
var _elm_lang$virtual_dom$VirtualDom_Debug$wrapWithFlags;

var _elm_lang$virtual_dom$Native_VirtualDom = function() {

var STYLE_KEY = 'STYLE';
var EVENT_KEY = 'EVENT';
var ATTR_KEY = 'ATTR';
var ATTR_NS_KEY = 'ATTR_NS';

var localDoc = typeof document !== 'undefined' ? document : {};


////////////  VIRTUAL DOM NODES  ////////////


function text(string)
{
	return {
		type: 'text',
		text: string
	};
}


function node(tag)
{
	return F2(function(factList, kidList) {
		return nodeHelp(tag, factList, kidList);
	});
}


function nodeHelp(tag, factList, kidList)
{
	var organized = organizeFacts(factList);
	var namespace = organized.namespace;
	var facts = organized.facts;

	var children = [];
	var descendantsCount = 0;
	while (kidList.ctor !== '[]')
	{
		var kid = kidList._0;
		descendantsCount += (kid.descendantsCount || 0);
		children.push(kid);
		kidList = kidList._1;
	}
	descendantsCount += children.length;

	return {
		type: 'node',
		tag: tag,
		facts: facts,
		children: children,
		namespace: namespace,
		descendantsCount: descendantsCount
	};
}


function keyedNode(tag, factList, kidList)
{
	var organized = organizeFacts(factList);
	var namespace = organized.namespace;
	var facts = organized.facts;

	var children = [];
	var descendantsCount = 0;
	while (kidList.ctor !== '[]')
	{
		var kid = kidList._0;
		descendantsCount += (kid._1.descendantsCount || 0);
		children.push(kid);
		kidList = kidList._1;
	}
	descendantsCount += children.length;

	return {
		type: 'keyed-node',
		tag: tag,
		facts: facts,
		children: children,
		namespace: namespace,
		descendantsCount: descendantsCount
	};
}


function custom(factList, model, impl)
{
	var facts = organizeFacts(factList).facts;

	return {
		type: 'custom',
		facts: facts,
		model: model,
		impl: impl
	};
}


function map(tagger, node)
{
	return {
		type: 'tagger',
		tagger: tagger,
		node: node,
		descendantsCount: 1 + (node.descendantsCount || 0)
	};
}


function thunk(func, args, thunk)
{
	return {
		type: 'thunk',
		func: func,
		args: args,
		thunk: thunk,
		node: undefined
	};
}

function lazy(fn, a)
{
	return thunk(fn, [a], function() {
		return fn(a);
	});
}

function lazy2(fn, a, b)
{
	return thunk(fn, [a,b], function() {
		return A2(fn, a, b);
	});
}

function lazy3(fn, a, b, c)
{
	return thunk(fn, [a,b,c], function() {
		return A3(fn, a, b, c);
	});
}



// FACTS


function organizeFacts(factList)
{
	var namespace, facts = {};

	while (factList.ctor !== '[]')
	{
		var entry = factList._0;
		var key = entry.key;

		if (key === ATTR_KEY || key === ATTR_NS_KEY || key === EVENT_KEY)
		{
			var subFacts = facts[key] || {};
			subFacts[entry.realKey] = entry.value;
			facts[key] = subFacts;
		}
		else if (key === STYLE_KEY)
		{
			var styles = facts[key] || {};
			var styleList = entry.value;
			while (styleList.ctor !== '[]')
			{
				var style = styleList._0;
				styles[style._0] = style._1;
				styleList = styleList._1;
			}
			facts[key] = styles;
		}
		else if (key === 'namespace')
		{
			namespace = entry.value;
		}
		else if (key === 'className')
		{
			var classes = facts[key];
			facts[key] = typeof classes === 'undefined'
				? entry.value
				: classes + ' ' + entry.value;
		}
 		else
		{
			facts[key] = entry.value;
		}
		factList = factList._1;
	}

	return {
		facts: facts,
		namespace: namespace
	};
}



////////////  PROPERTIES AND ATTRIBUTES  ////////////


function style(value)
{
	return {
		key: STYLE_KEY,
		value: value
	};
}


function property(key, value)
{
	return {
		key: key,
		value: value
	};
}


function attribute(key, value)
{
	return {
		key: ATTR_KEY,
		realKey: key,
		value: value
	};
}


function attributeNS(namespace, key, value)
{
	return {
		key: ATTR_NS_KEY,
		realKey: key,
		value: {
			value: value,
			namespace: namespace
		}
	};
}


function on(name, options, decoder)
{
	return {
		key: EVENT_KEY,
		realKey: name,
		value: {
			options: options,
			decoder: decoder
		}
	};
}


function equalEvents(a, b)
{
	if (a.options !== b.options)
	{
		if (a.options.stopPropagation !== b.options.stopPropagation || a.options.preventDefault !== b.options.preventDefault)
		{
			return false;
		}
	}
	return _elm_lang$core$Native_Json.equality(a.decoder, b.decoder);
}


function mapProperty(func, property)
{
	if (property.key !== EVENT_KEY)
	{
		return property;
	}
	return on(
		property.realKey,
		property.value.options,
		A2(_elm_lang$core$Json_Decode$map, func, property.value.decoder)
	);
}


////////////  RENDER  ////////////


function render(vNode, eventNode)
{
	switch (vNode.type)
	{
		case 'thunk':
			if (!vNode.node)
			{
				vNode.node = vNode.thunk();
			}
			return render(vNode.node, eventNode);

		case 'tagger':
			var subNode = vNode.node;
			var tagger = vNode.tagger;

			while (subNode.type === 'tagger')
			{
				typeof tagger !== 'object'
					? tagger = [tagger, subNode.tagger]
					: tagger.push(subNode.tagger);

				subNode = subNode.node;
			}

			var subEventRoot = { tagger: tagger, parent: eventNode };
			var domNode = render(subNode, subEventRoot);
			domNode.elm_event_node_ref = subEventRoot;
			return domNode;

		case 'text':
			return localDoc.createTextNode(vNode.text);

		case 'node':
			var domNode = vNode.namespace
				? localDoc.createElementNS(vNode.namespace, vNode.tag)
				: localDoc.createElement(vNode.tag);

			applyFacts(domNode, eventNode, vNode.facts);

			var children = vNode.children;

			for (var i = 0; i < children.length; i++)
			{
				domNode.appendChild(render(children[i], eventNode));
			}

			return domNode;

		case 'keyed-node':
			var domNode = vNode.namespace
				? localDoc.createElementNS(vNode.namespace, vNode.tag)
				: localDoc.createElement(vNode.tag);

			applyFacts(domNode, eventNode, vNode.facts);

			var children = vNode.children;

			for (var i = 0; i < children.length; i++)
			{
				domNode.appendChild(render(children[i]._1, eventNode));
			}

			return domNode;

		case 'custom':
			var domNode = vNode.impl.render(vNode.model);
			applyFacts(domNode, eventNode, vNode.facts);
			return domNode;
	}
}



////////////  APPLY FACTS  ////////////


function applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		switch (key)
		{
			case STYLE_KEY:
				applyStyles(domNode, value);
				break;

			case EVENT_KEY:
				applyEvents(domNode, eventNode, value);
				break;

			case ATTR_KEY:
				applyAttrs(domNode, value);
				break;

			case ATTR_NS_KEY:
				applyAttrsNS(domNode, value);
				break;

			case 'value':
				if (domNode[key] !== value)
				{
					domNode[key] = value;
				}
				break;

			default:
				domNode[key] = value;
				break;
		}
	}
}

function applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}

function applyEvents(domNode, eventNode, events)
{
	var allHandlers = domNode.elm_handlers || {};

	for (var key in events)
	{
		var handler = allHandlers[key];
		var value = events[key];

		if (typeof value === 'undefined')
		{
			domNode.removeEventListener(key, handler);
			allHandlers[key] = undefined;
		}
		else if (typeof handler === 'undefined')
		{
			var handler = makeEventHandler(eventNode, value);
			domNode.addEventListener(key, handler);
			allHandlers[key] = handler;
		}
		else
		{
			handler.info = value;
		}
	}

	domNode.elm_handlers = allHandlers;
}

function makeEventHandler(eventNode, info)
{
	function eventHandler(event)
	{
		var info = eventHandler.info;

		var value = A2(_elm_lang$core$Native_Json.run, info.decoder, event);

		if (value.ctor === 'Ok')
		{
			var options = info.options;
			if (options.stopPropagation)
			{
				event.stopPropagation();
			}
			if (options.preventDefault)
			{
				event.preventDefault();
			}

			var message = value._0;

			var currentEventNode = eventNode;
			while (currentEventNode)
			{
				var tagger = currentEventNode.tagger;
				if (typeof tagger === 'function')
				{
					message = tagger(message);
				}
				else
				{
					for (var i = tagger.length; i--; )
					{
						message = tagger[i](message);
					}
				}
				currentEventNode = currentEventNode.parent;
			}
		}
	};

	eventHandler.info = info;

	return eventHandler;
}

function applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		if (typeof value === 'undefined')
		{
			domNode.removeAttribute(key);
		}
		else
		{
			domNode.setAttribute(key, value);
		}
	}
}

function applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.namespace;
		var value = pair.value;

		if (typeof value === 'undefined')
		{
			domNode.removeAttributeNS(namespace, key);
		}
		else
		{
			domNode.setAttributeNS(namespace, key, value);
		}
	}
}



////////////  DIFF  ////////////


function diff(a, b)
{
	var patches = [];
	diffHelp(a, b, patches, 0);
	return patches;
}


function makePatch(type, index, data)
{
	return {
		index: index,
		type: type,
		data: data,
		domNode: undefined,
		eventNode: undefined
	};
}


function diffHelp(a, b, patches, index)
{
	if (a === b)
	{
		return;
	}

	var aType = a.type;
	var bType = b.type;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (aType !== bType)
	{
		patches.push(makePatch('p-redraw', index, b));
		return;
	}

	// Now we know that both nodes are the same type.
	switch (bType)
	{
		case 'thunk':
			var aArgs = a.args;
			var bArgs = b.args;
			var i = aArgs.length;
			var same = a.func === b.func && i === bArgs.length;
			while (same && i--)
			{
				same = aArgs[i] === bArgs[i];
			}
			if (same)
			{
				b.node = a.node;
				return;
			}
			b.node = b.thunk();
			var subPatches = [];
			diffHelp(a.node, b.node, subPatches, 0);
			if (subPatches.length > 0)
			{
				patches.push(makePatch('p-thunk', index, subPatches));
			}
			return;

		case 'tagger':
			// gather nested taggers
			var aTaggers = a.tagger;
			var bTaggers = b.tagger;
			var nesting = false;

			var aSubNode = a.node;
			while (aSubNode.type === 'tagger')
			{
				nesting = true;

				typeof aTaggers !== 'object'
					? aTaggers = [aTaggers, aSubNode.tagger]
					: aTaggers.push(aSubNode.tagger);

				aSubNode = aSubNode.node;
			}

			var bSubNode = b.node;
			while (bSubNode.type === 'tagger')
			{
				nesting = true;

				typeof bTaggers !== 'object'
					? bTaggers = [bTaggers, bSubNode.tagger]
					: bTaggers.push(bSubNode.tagger);

				bSubNode = bSubNode.node;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && aTaggers.length !== bTaggers.length)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !pairwiseRefEqual(aTaggers, bTaggers) : aTaggers !== bTaggers)
			{
				patches.push(makePatch('p-tagger', index, bTaggers));
			}

			// diff everything below the taggers
			diffHelp(aSubNode, bSubNode, patches, index + 1);
			return;

		case 'text':
			if (a.text !== b.text)
			{
				patches.push(makePatch('p-text', index, b.text));
				return;
			}

			return;

		case 'node':
			// Bail if obvious indicators have changed. Implies more serious
			// structural changes such that it's not worth it to diff.
			if (a.tag !== b.tag || a.namespace !== b.namespace)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			var factsDiff = diffFacts(a.facts, b.facts);

			if (typeof factsDiff !== 'undefined')
			{
				patches.push(makePatch('p-facts', index, factsDiff));
			}

			diffChildren(a, b, patches, index);
			return;

		case 'keyed-node':
			// Bail if obvious indicators have changed. Implies more serious
			// structural changes such that it's not worth it to diff.
			if (a.tag !== b.tag || a.namespace !== b.namespace)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			var factsDiff = diffFacts(a.facts, b.facts);

			if (typeof factsDiff !== 'undefined')
			{
				patches.push(makePatch('p-facts', index, factsDiff));
			}

			diffKeyedChildren(a, b, patches, index);
			return;

		case 'custom':
			if (a.impl !== b.impl)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			var factsDiff = diffFacts(a.facts, b.facts);
			if (typeof factsDiff !== 'undefined')
			{
				patches.push(makePatch('p-facts', index, factsDiff));
			}

			var patch = b.impl.diff(a,b);
			if (patch)
			{
				patches.push(makePatch('p-custom', index, patch));
				return;
			}

			return;
	}
}


// assumes the incoming arrays are the same length
function pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function diffFacts(a, b, category)
{
	var diff;

	// look for changes and removals
	for (var aKey in a)
	{
		if (aKey === STYLE_KEY || aKey === EVENT_KEY || aKey === ATTR_KEY || aKey === ATTR_NS_KEY)
		{
			var subDiff = diffFacts(a[aKey], b[aKey] || {}, aKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[aKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(aKey in b))
		{
			diff = diff || {};
			diff[aKey] =
				(typeof category === 'undefined')
					? (typeof a[aKey] === 'string' ? '' : null)
					:
				(category === STYLE_KEY)
					? ''
					:
				(category === EVENT_KEY || category === ATTR_KEY)
					? undefined
					:
				{ namespace: a[aKey].namespace, value: undefined };

			continue;
		}

		var aValue = a[aKey];
		var bValue = b[aKey];

		// reference equal, so don't worry about it
		if (aValue === bValue && aKey !== 'value'
			|| category === EVENT_KEY && equalEvents(aValue, bValue))
		{
			continue;
		}

		diff = diff || {};
		diff[aKey] = bValue;
	}

	// add new stuff
	for (var bKey in b)
	{
		if (!(bKey in a))
		{
			diff = diff || {};
			diff[bKey] = b[bKey];
		}
	}

	return diff;
}


function diffChildren(aParent, bParent, patches, rootIndex)
{
	var aChildren = aParent.children;
	var bChildren = bParent.children;

	var aLen = aChildren.length;
	var bLen = bChildren.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (aLen > bLen)
	{
		patches.push(makePatch('p-remove-last', rootIndex, aLen - bLen));
	}
	else if (aLen < bLen)
	{
		patches.push(makePatch('p-append', rootIndex, bChildren.slice(aLen)));
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	var index = rootIndex;
	var minLen = aLen < bLen ? aLen : bLen;
	for (var i = 0; i < minLen; i++)
	{
		index++;
		var aChild = aChildren[i];
		diffHelp(aChild, bChildren[i], patches, index);
		index += aChild.descendantsCount || 0;
	}
}



////////////  KEYED DIFF  ////////////


function diffKeyedChildren(aParent, bParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var aChildren = aParent.children;
	var bChildren = bParent.children;
	var aLen = aChildren.length;
	var bLen = bChildren.length;
	var aIndex = 0;
	var bIndex = 0;

	var index = rootIndex;

	while (aIndex < aLen && bIndex < bLen)
	{
		var a = aChildren[aIndex];
		var b = bChildren[bIndex];

		var aKey = a._0;
		var bKey = b._0;
		var aNode = a._1;
		var bNode = b._1;

		// check if keys match

		if (aKey === bKey)
		{
			index++;
			diffHelp(aNode, bNode, localPatches, index);
			index += aNode.descendantsCount || 0;

			aIndex++;
			bIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var aLookAhead = aIndex + 1 < aLen;
		var bLookAhead = bIndex + 1 < bLen;

		if (aLookAhead)
		{
			var aNext = aChildren[aIndex + 1];
			var aNextKey = aNext._0;
			var aNextNode = aNext._1;
			var oldMatch = bKey === aNextKey;
		}

		if (bLookAhead)
		{
			var bNext = bChildren[bIndex + 1];
			var bNextKey = bNext._0;
			var bNextNode = bNext._1;
			var newMatch = aKey === bNextKey;
		}


		// swap a and b
		if (aLookAhead && bLookAhead && newMatch && oldMatch)
		{
			index++;
			diffHelp(aNode, bNextNode, localPatches, index);
			insertNode(changes, localPatches, aKey, bNode, bIndex, inserts);
			index += aNode.descendantsCount || 0;

			index++;
			removeNode(changes, localPatches, aKey, aNextNode, index);
			index += aNextNode.descendantsCount || 0;

			aIndex += 2;
			bIndex += 2;
			continue;
		}

		// insert b
		if (bLookAhead && newMatch)
		{
			index++;
			insertNode(changes, localPatches, bKey, bNode, bIndex, inserts);
			diffHelp(aNode, bNextNode, localPatches, index);
			index += aNode.descendantsCount || 0;

			aIndex += 1;
			bIndex += 2;
			continue;
		}

		// remove a
		if (aLookAhead && oldMatch)
		{
			index++;
			removeNode(changes, localPatches, aKey, aNode, index);
			index += aNode.descendantsCount || 0;

			index++;
			diffHelp(aNextNode, bNode, localPatches, index);
			index += aNextNode.descendantsCount || 0;

			aIndex += 2;
			bIndex += 1;
			continue;
		}

		// remove a, insert b
		if (aLookAhead && bLookAhead && aNextKey === bNextKey)
		{
			index++;
			removeNode(changes, localPatches, aKey, aNode, index);
			insertNode(changes, localPatches, bKey, bNode, bIndex, inserts);
			index += aNode.descendantsCount || 0;

			index++;
			diffHelp(aNextNode, bNextNode, localPatches, index);
			index += aNextNode.descendantsCount || 0;

			aIndex += 2;
			bIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (aIndex < aLen)
	{
		index++;
		var a = aChildren[aIndex];
		var aNode = a._1;
		removeNode(changes, localPatches, a._0, aNode, index);
		index += aNode.descendantsCount || 0;
		aIndex++;
	}

	var endInserts;
	while (bIndex < bLen)
	{
		endInserts = endInserts || [];
		var b = bChildren[bIndex];
		insertNode(changes, localPatches, b._0, b._1, undefined, endInserts);
		bIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || typeof endInserts !== 'undefined')
	{
		patches.push(makePatch('p-reorder', rootIndex, {
			patches: localPatches,
			inserts: inserts,
			endInserts: endInserts
		}));
	}
}



////////////  CHANGES FROM KEYED DIFF  ////////////


var POSTFIX = '_elmW6BL';


function insertNode(changes, localPatches, key, vnode, bIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (typeof entry === 'undefined')
	{
		entry = {
			tag: 'insert',
			vnode: vnode,
			index: bIndex,
			data: undefined
		};

		inserts.push({ index: bIndex, entry: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.tag === 'remove')
	{
		inserts.push({ index: bIndex, entry: entry });

		entry.tag = 'move';
		var subPatches = [];
		diffHelp(entry.vnode, vnode, subPatches, entry.index);
		entry.index = bIndex;
		entry.data.data = {
			patches: subPatches,
			entry: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	insertNode(changes, localPatches, key + POSTFIX, vnode, bIndex, inserts);
}


function removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (typeof entry === 'undefined')
	{
		var patch = makePatch('p-remove', index, undefined);
		localPatches.push(patch);

		changes[key] = {
			tag: 'remove',
			vnode: vnode,
			index: index,
			data: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.tag === 'insert')
	{
		entry.tag = 'move';
		var subPatches = [];
		diffHelp(vnode, entry.vnode, subPatches, index);

		var patch = makePatch('p-remove', index, {
			patches: subPatches,
			entry: entry
		});
		localPatches.push(patch);

		return;
	}

	// this key has already been removed or moved, a duplicate!
	removeNode(changes, localPatches, key + POSTFIX, vnode, index);
}



////////////  ADD DOM NODES  ////////////
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function addDomNodes(domNode, vNode, patches, eventNode)
{
	addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.descendantsCount, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.index;

	while (index === low)
	{
		var patchType = patch.type;

		if (patchType === 'p-thunk')
		{
			addDomNodes(domNode, vNode.node, patch.data, eventNode);
		}
		else if (patchType === 'p-reorder')
		{
			patch.domNode = domNode;
			patch.eventNode = eventNode;

			var subPatches = patch.data.patches;
			if (subPatches.length > 0)
			{
				addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 'p-remove')
		{
			patch.domNode = domNode;
			patch.eventNode = eventNode;

			var data = patch.data;
			if (typeof data !== 'undefined')
			{
				data.entry.data = domNode;
				var subPatches = data.patches;
				if (subPatches.length > 0)
				{
					addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.domNode = domNode;
			patch.eventNode = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.index) > high)
		{
			return i;
		}
	}

	switch (vNode.type)
	{
		case 'tagger':
			var subNode = vNode.node;

			while (subNode.type === "tagger")
			{
				subNode = subNode.node;
			}

			return addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);

		case 'node':
			var vChildren = vNode.children;
			var childNodes = domNode.childNodes;
			for (var j = 0; j < vChildren.length; j++)
			{
				low++;
				var vChild = vChildren[j];
				var nextLow = low + (vChild.descendantsCount || 0);
				if (low <= index && index <= nextLow)
				{
					i = addDomNodesHelp(childNodes[j], vChild, patches, i, low, nextLow, eventNode);
					if (!(patch = patches[i]) || (index = patch.index) > high)
					{
						return i;
					}
				}
				low = nextLow;
			}
			return i;

		case 'keyed-node':
			var vChildren = vNode.children;
			var childNodes = domNode.childNodes;
			for (var j = 0; j < vChildren.length; j++)
			{
				low++;
				var vChild = vChildren[j]._1;
				var nextLow = low + (vChild.descendantsCount || 0);
				if (low <= index && index <= nextLow)
				{
					i = addDomNodesHelp(childNodes[j], vChild, patches, i, low, nextLow, eventNode);
					if (!(patch = patches[i]) || (index = patch.index) > high)
					{
						return i;
					}
				}
				low = nextLow;
			}
			return i;

		case 'text':
		case 'thunk':
			throw new Error('should never traverse `text` or `thunk` nodes like this');
	}
}



////////////  APPLY PATCHES  ////////////


function applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return applyPatchesHelp(rootDomNode, patches);
}

function applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.domNode
		var newNode = applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function applyPatch(domNode, patch)
{
	switch (patch.type)
	{
		case 'p-redraw':
			return applyPatchRedraw(domNode, patch.data, patch.eventNode);

		case 'p-facts':
			applyFacts(domNode, patch.eventNode, patch.data);
			return domNode;

		case 'p-text':
			domNode.replaceData(0, domNode.length, patch.data);
			return domNode;

		case 'p-thunk':
			return applyPatchesHelp(domNode, patch.data);

		case 'p-tagger':
			if (typeof domNode.elm_event_node_ref !== 'undefined')
			{
				domNode.elm_event_node_ref.tagger = patch.data;
			}
			else
			{
				domNode.elm_event_node_ref = { tagger: patch.data, parent: patch.eventNode };
			}
			return domNode;

		case 'p-remove-last':
			var i = patch.data;
			while (i--)
			{
				domNode.removeChild(domNode.lastChild);
			}
			return domNode;

		case 'p-append':
			var newNodes = patch.data;
			for (var i = 0; i < newNodes.length; i++)
			{
				domNode.appendChild(render(newNodes[i], patch.eventNode));
			}
			return domNode;

		case 'p-remove':
			var data = patch.data;
			if (typeof data === 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.entry;
			if (typeof entry.index !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.data = applyPatchesHelp(domNode, data.patches);
			return domNode;

		case 'p-reorder':
			return applyPatchReorder(domNode, patch);

		case 'p-custom':
			var impl = patch.data;
			return impl.applyPatch(domNode, impl.data);

		default:
			throw new Error('Ran into an unknown patch!');
	}
}


function applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = render(vNode, eventNode);

	if (typeof newNode.elm_event_node_ref === 'undefined')
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function applyPatchReorder(domNode, patch)
{
	var data = patch.data;

	// remove end inserts
	var frag = applyPatchReorderEndInsertsHelp(data.endInserts, patch);

	// removals
	domNode = applyPatchesHelp(domNode, data.patches);

	// inserts
	var inserts = data.inserts;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.entry;
		var node = entry.tag === 'move'
			? entry.data
			: render(entry.vnode, patch.eventNode);
		domNode.insertBefore(node, domNode.childNodes[insert.index]);
	}

	// add end inserts
	if (typeof frag !== 'undefined')
	{
		domNode.appendChild(frag);
	}

	return domNode;
}


function applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (typeof endInserts === 'undefined')
	{
		return;
	}

	var frag = localDoc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.entry;
		frag.appendChild(entry.tag === 'move'
			? entry.data
			: render(entry.vnode, patch.eventNode)
		);
	}
	return frag;
}


// PROGRAMS

var program = makeProgram(checkNoFlags);
var programWithFlags = makeProgram(checkYesFlags);

function makeProgram(flagChecker)
{
	return F2(function(debugWrap, impl)
	{
		return function(flagDecoder)
		{
			return function(object, moduleName, debugMetadata)
			{
				var checker = flagChecker(flagDecoder, moduleName);
				if (typeof debugMetadata === 'undefined')
				{
					normalSetup(impl, object, moduleName, checker);
				}
				else
				{
					debugSetup(A2(debugWrap, debugMetadata, impl), object, moduleName, checker);
				}
			};
		};
	});
}

function staticProgram(vNode)
{
	var nothing = _elm_lang$core$Native_Utils.Tuple2(
		_elm_lang$core$Native_Utils.Tuple0,
		_elm_lang$core$Platform_Cmd$none
	);
	return A2(program, _elm_lang$virtual_dom$VirtualDom_Debug$wrap, {
		init: nothing,
		view: function() { return vNode; },
		update: F2(function() { return nothing; }),
		subscriptions: function() { return _elm_lang$core$Platform_Sub$none; }
	})();
}


// FLAG CHECKERS

function checkNoFlags(flagDecoder, moduleName)
{
	return function(init, flags, domNode)
	{
		if (typeof flags === 'undefined')
		{
			return init;
		}

		var errorMessage =
			'The `' + moduleName + '` module does not need flags.\n'
			+ 'Initialize it with no arguments and you should be all set!';

		crash(errorMessage, domNode);
	};
}

function checkYesFlags(flagDecoder, moduleName)
{
	return function(init, flags, domNode)
	{
		if (typeof flagDecoder === 'undefined')
		{
			var errorMessage =
				'Are you trying to sneak a Never value into Elm? Trickster!\n'
				+ 'It looks like ' + moduleName + '.main is defined with `programWithFlags` but has type `Program Never`.\n'
				+ 'Use `program` instead if you do not want flags.'

			crash(errorMessage, domNode);
		}

		var result = A2(_elm_lang$core$Native_Json.run, flagDecoder, flags);
		if (result.ctor === 'Ok')
		{
			return init(result._0);
		}

		var errorMessage =
			'Trying to initialize the `' + moduleName + '` module with an unexpected flag.\n'
			+ 'I tried to convert it to an Elm value, but ran into this problem:\n\n'
			+ result._0;

		crash(errorMessage, domNode);
	};
}

function crash(errorMessage, domNode)
{
	if (domNode)
	{
		domNode.innerHTML =
			'<div style="padding-left:1em;">'
			+ '<h2 style="font-weight:normal;"><b>Oops!</b> Something went wrong when starting your Elm program.</h2>'
			+ '<pre style="padding-left:1em;">' + errorMessage + '</pre>'
			+ '</div>';
	}

	throw new Error(errorMessage);
}


//  NORMAL SETUP

function normalSetup(impl, object, moduleName, flagChecker)
{
	object['embed'] = function embed(node, flags)
	{
		while (node.lastChild)
		{
			node.removeChild(node.lastChild);
		}

		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, node),
			impl.update,
			impl.subscriptions,
			normalRenderer(node, impl.view)
		);
	};

	object['fullscreen'] = function fullscreen(flags)
	{
		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, document.body),
			impl.update,
			impl.subscriptions,
			normalRenderer(document.body, impl.view)
		);
	};
}

function normalRenderer(parentNode, view)
{
	return function(tagger, initialModel)
	{
		var eventNode = { tagger: tagger, parent: undefined };
		var initialVirtualNode = view(initialModel);
		var domNode = render(initialVirtualNode, eventNode);
		parentNode.appendChild(domNode);
		return makeStepper(domNode, view, initialVirtualNode, eventNode);
	};
}


// STEPPER

var rAF =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { setTimeout(callback, 1000 / 60); };

function makeStepper(domNode, view, initialVirtualNode, eventNode)
{
	var state = 'NO_REQUEST';
	var currNode = initialVirtualNode;
	var nextModel;

	function updateIfNeeded()
	{
		switch (state)
		{
			case 'NO_REQUEST':
				throw new Error(
					'Unexpected draw callback.\n' +
					'Please report this to <https://github.com/elm-lang/virtual-dom/issues>.'
				);

			case 'PENDING_REQUEST':
				rAF(updateIfNeeded);
				state = 'EXTRA_REQUEST';

				var nextNode = view(nextModel);
				var patches = diff(currNode, nextNode);
				domNode = applyPatches(domNode, currNode, patches, eventNode);
				currNode = nextNode;

				return;

			case 'EXTRA_REQUEST':
				state = 'NO_REQUEST';
				return;
		}
	}

	return function stepper(model)
	{
		if (state === 'NO_REQUEST')
		{
			rAF(updateIfNeeded);
		}
		state = 'PENDING_REQUEST';
		nextModel = model;
	};
}


// DEBUG SETUP

function debugSetup(impl, object, moduleName, flagChecker)
{
	object['fullscreen'] = function fullscreen(flags)
	{
		var popoutRef = { doc: undefined };
		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, document.body),
			impl.update(scrollTask(popoutRef)),
			impl.subscriptions,
			debugRenderer(moduleName, document.body, popoutRef, impl.view, impl.viewIn, impl.viewOut)
		);
	};

	object['embed'] = function fullscreen(node, flags)
	{
		var popoutRef = { doc: undefined };
		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, node),
			impl.update(scrollTask(popoutRef)),
			impl.subscriptions,
			debugRenderer(moduleName, node, popoutRef, impl.view, impl.viewIn, impl.viewOut)
		);
	};
}

function scrollTask(popoutRef)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		var doc = popoutRef.doc;
		if (doc)
		{
			var msgs = doc.getElementsByClassName('debugger-sidebar-messages')[0];
			if (msgs)
			{
				msgs.scrollTop = msgs.scrollHeight;
			}
		}
		callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}


function debugRenderer(moduleName, parentNode, popoutRef, view, viewIn, viewOut)
{
	return function(tagger, initialModel)
	{
		var appEventNode = { tagger: tagger, parent: undefined };
		var eventNode = { tagger: tagger, parent: undefined };

		// make normal stepper
		var appVirtualNode = view(initialModel);
		var appNode = render(appVirtualNode, appEventNode);
		parentNode.appendChild(appNode);
		var appStepper = makeStepper(appNode, view, appVirtualNode, appEventNode);

		// make overlay stepper
		var overVirtualNode = viewIn(initialModel)._1;
		var overNode = render(overVirtualNode, eventNode);
		parentNode.appendChild(overNode);
		var wrappedViewIn = wrapViewIn(appEventNode, overNode, viewIn);
		var overStepper = makeStepper(overNode, wrappedViewIn, overVirtualNode, eventNode);

		// make debugger stepper
		var debugStepper = makeDebugStepper(initialModel, viewOut, eventNode, parentNode, moduleName, popoutRef);

		return function stepper(model)
		{
			appStepper(model);
			overStepper(model);
			debugStepper(model);
		}
	};
}

function makeDebugStepper(initialModel, view, eventNode, parentNode, moduleName, popoutRef)
{
	var curr;
	var domNode;

	return function stepper(model)
	{
		if (!model.isDebuggerOpen)
		{
			return;
		}

		if (!popoutRef.doc)
		{
			curr = view(model);
			domNode = openDebugWindow(moduleName, popoutRef, curr, eventNode);
			return;
		}

		// switch to document of popout
		localDoc = popoutRef.doc;

		var next = view(model);
		var patches = diff(curr, next);
		domNode = applyPatches(domNode, curr, patches, eventNode);
		curr = next;

		// switch back to normal document
		localDoc = document;
	};
}

function openDebugWindow(moduleName, popoutRef, virtualNode, eventNode)
{
	var w = 900;
	var h = 360;
	var x = screen.width - w;
	var y = screen.height - h;
	var debugWindow = window.open('', '', 'width=' + w + ',height=' + h + ',left=' + x + ',top=' + y);

	// switch to window document
	localDoc = debugWindow.document;

	popoutRef.doc = localDoc;
	localDoc.title = 'Debugger - ' + moduleName;
	localDoc.body.style.margin = '0';
	localDoc.body.style.padding = '0';
	var domNode = render(virtualNode, eventNode);
	localDoc.body.appendChild(domNode);

	localDoc.addEventListener('keydown', function(event) {
		if (event.metaKey && event.which === 82)
		{
			window.location.reload();
		}
		if (event.which === 38)
		{
			eventNode.tagger({ ctor: 'Up' });
			event.preventDefault();
		}
		if (event.which === 40)
		{
			eventNode.tagger({ ctor: 'Down' });
			event.preventDefault();
		}
	});

	function close()
	{
		popoutRef.doc = undefined;
		debugWindow.close();
	}
	window.addEventListener('unload', close);
	debugWindow.addEventListener('unload', function() {
		popoutRef.doc = undefined;
		window.removeEventListener('unload', close);
		eventNode.tagger({ ctor: 'Close' });
	});

	// switch back to the normal document
	localDoc = document;

	return domNode;
}


// BLOCK EVENTS

function wrapViewIn(appEventNode, overlayNode, viewIn)
{
	var ignorer = makeIgnorer(overlayNode);
	var blocking = 'Normal';
	var overflow;

	var normalTagger = appEventNode.tagger;
	var blockTagger = function() {};

	return function(model)
	{
		var tuple = viewIn(model);
		var newBlocking = tuple._0.ctor;
		appEventNode.tagger = newBlocking === 'Normal' ? normalTagger : blockTagger;
		if (blocking !== newBlocking)
		{
			traverse('removeEventListener', ignorer, blocking);
			traverse('addEventListener', ignorer, newBlocking);

			if (blocking === 'Normal')
			{
				overflow = document.body.style.overflow;
				document.body.style.overflow = 'hidden';
			}

			if (newBlocking === 'Normal')
			{
				document.body.style.overflow = overflow;
			}

			blocking = newBlocking;
		}
		return tuple._1;
	}
}

function traverse(verbEventListener, ignorer, blocking)
{
	switch(blocking)
	{
		case 'Normal':
			return;

		case 'Pause':
			return traverseHelp(verbEventListener, ignorer, mostEvents);

		case 'Message':
			return traverseHelp(verbEventListener, ignorer, allEvents);
	}
}

function traverseHelp(verbEventListener, handler, eventNames)
{
	for (var i = 0; i < eventNames.length; i++)
	{
		document.body[verbEventListener](eventNames[i], handler, true);
	}
}

function makeIgnorer(overlayNode)
{
	return function(event)
	{
		if (event.type === 'keydown' && event.metaKey && event.which === 82)
		{
			return;
		}

		var isScroll = event.type === 'scroll' || event.type === 'wheel';

		var node = event.target;
		while (node !== null)
		{
			if (node.className === 'elm-overlay-message-details' && isScroll)
			{
				return;
			}

			if (node === overlayNode && !isScroll)
			{
				return;
			}
			node = node.parentNode;
		}

		event.stopPropagation();
		event.preventDefault();
	}
}

var mostEvents = [
	'click', 'dblclick', 'mousemove',
	'mouseup', 'mousedown', 'mouseenter', 'mouseleave',
	'touchstart', 'touchend', 'touchcancel', 'touchmove',
	'pointerdown', 'pointerup', 'pointerover', 'pointerout',
	'pointerenter', 'pointerleave', 'pointermove', 'pointercancel',
	'dragstart', 'drag', 'dragend', 'dragenter', 'dragover', 'dragleave', 'drop',
	'keyup', 'keydown', 'keypress',
	'input', 'change',
	'focus', 'blur'
];

var allEvents = mostEvents.concat('wheel', 'scroll');


return {
	node: node,
	text: text,
	custom: custom,
	map: F2(map),

	on: F3(on),
	style: style,
	property: F2(property),
	attribute: F2(attribute),
	attributeNS: F3(attributeNS),
	mapProperty: F2(mapProperty),

	lazy: F2(lazy),
	lazy2: F3(lazy2),
	lazy3: F4(lazy3),
	keyedNode: F3(keyedNode),

	program: program,
	programWithFlags: programWithFlags,
	staticProgram: staticProgram
};

}();

var _elm_lang$virtual_dom$VirtualDom$programWithFlags = function (impl) {
	return A2(_elm_lang$virtual_dom$Native_VirtualDom.programWithFlags, _elm_lang$virtual_dom$VirtualDom_Debug$wrapWithFlags, impl);
};
var _elm_lang$virtual_dom$VirtualDom$program = function (impl) {
	return A2(_elm_lang$virtual_dom$Native_VirtualDom.program, _elm_lang$virtual_dom$VirtualDom_Debug$wrap, impl);
};
var _elm_lang$virtual_dom$VirtualDom$keyedNode = _elm_lang$virtual_dom$Native_VirtualDom.keyedNode;
var _elm_lang$virtual_dom$VirtualDom$lazy3 = _elm_lang$virtual_dom$Native_VirtualDom.lazy3;
var _elm_lang$virtual_dom$VirtualDom$lazy2 = _elm_lang$virtual_dom$Native_VirtualDom.lazy2;
var _elm_lang$virtual_dom$VirtualDom$lazy = _elm_lang$virtual_dom$Native_VirtualDom.lazy;
var _elm_lang$virtual_dom$VirtualDom$defaultOptions = {stopPropagation: false, preventDefault: false};
var _elm_lang$virtual_dom$VirtualDom$onWithOptions = _elm_lang$virtual_dom$Native_VirtualDom.on;
var _elm_lang$virtual_dom$VirtualDom$on = F2(
	function (eventName, decoder) {
		return A3(_elm_lang$virtual_dom$VirtualDom$onWithOptions, eventName, _elm_lang$virtual_dom$VirtualDom$defaultOptions, decoder);
	});
var _elm_lang$virtual_dom$VirtualDom$style = _elm_lang$virtual_dom$Native_VirtualDom.style;
var _elm_lang$virtual_dom$VirtualDom$mapProperty = _elm_lang$virtual_dom$Native_VirtualDom.mapProperty;
var _elm_lang$virtual_dom$VirtualDom$attributeNS = _elm_lang$virtual_dom$Native_VirtualDom.attributeNS;
var _elm_lang$virtual_dom$VirtualDom$attribute = _elm_lang$virtual_dom$Native_VirtualDom.attribute;
var _elm_lang$virtual_dom$VirtualDom$property = _elm_lang$virtual_dom$Native_VirtualDom.property;
var _elm_lang$virtual_dom$VirtualDom$map = _elm_lang$virtual_dom$Native_VirtualDom.map;
var _elm_lang$virtual_dom$VirtualDom$text = _elm_lang$virtual_dom$Native_VirtualDom.text;
var _elm_lang$virtual_dom$VirtualDom$node = _elm_lang$virtual_dom$Native_VirtualDom.node;
var _elm_lang$virtual_dom$VirtualDom$Options = F2(
	function (a, b) {
		return {stopPropagation: a, preventDefault: b};
	});
var _elm_lang$virtual_dom$VirtualDom$Node = {ctor: 'Node'};
var _elm_lang$virtual_dom$VirtualDom$Property = {ctor: 'Property'};

var _elm_lang$html$Html$programWithFlags = _elm_lang$virtual_dom$VirtualDom$programWithFlags;
var _elm_lang$html$Html$program = _elm_lang$virtual_dom$VirtualDom$program;
var _elm_lang$html$Html$beginnerProgram = function (_p0) {
	var _p1 = _p0;
	return _elm_lang$html$Html$program(
		{
			init: A2(
				_elm_lang$core$Platform_Cmd_ops['!'],
				_p1.model,
				{ctor: '[]'}),
			update: F2(
				function (msg, model) {
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						A2(_p1.update, msg, model),
						{ctor: '[]'});
				}),
			view: _p1.view,
			subscriptions: function (_p2) {
				return _elm_lang$core$Platform_Sub$none;
			}
		});
};
var _elm_lang$html$Html$map = _elm_lang$virtual_dom$VirtualDom$map;
var _elm_lang$html$Html$text = _elm_lang$virtual_dom$VirtualDom$text;
var _elm_lang$html$Html$node = _elm_lang$virtual_dom$VirtualDom$node;
var _elm_lang$html$Html$body = _elm_lang$html$Html$node('body');
var _elm_lang$html$Html$section = _elm_lang$html$Html$node('section');
var _elm_lang$html$Html$nav = _elm_lang$html$Html$node('nav');
var _elm_lang$html$Html$article = _elm_lang$html$Html$node('article');
var _elm_lang$html$Html$aside = _elm_lang$html$Html$node('aside');
var _elm_lang$html$Html$h1 = _elm_lang$html$Html$node('h1');
var _elm_lang$html$Html$h2 = _elm_lang$html$Html$node('h2');
var _elm_lang$html$Html$h3 = _elm_lang$html$Html$node('h3');
var _elm_lang$html$Html$h4 = _elm_lang$html$Html$node('h4');
var _elm_lang$html$Html$h5 = _elm_lang$html$Html$node('h5');
var _elm_lang$html$Html$h6 = _elm_lang$html$Html$node('h6');
var _elm_lang$html$Html$header = _elm_lang$html$Html$node('header');
var _elm_lang$html$Html$footer = _elm_lang$html$Html$node('footer');
var _elm_lang$html$Html$address = _elm_lang$html$Html$node('address');
var _elm_lang$html$Html$main_ = _elm_lang$html$Html$node('main');
var _elm_lang$html$Html$p = _elm_lang$html$Html$node('p');
var _elm_lang$html$Html$hr = _elm_lang$html$Html$node('hr');
var _elm_lang$html$Html$pre = _elm_lang$html$Html$node('pre');
var _elm_lang$html$Html$blockquote = _elm_lang$html$Html$node('blockquote');
var _elm_lang$html$Html$ol = _elm_lang$html$Html$node('ol');
var _elm_lang$html$Html$ul = _elm_lang$html$Html$node('ul');
var _elm_lang$html$Html$li = _elm_lang$html$Html$node('li');
var _elm_lang$html$Html$dl = _elm_lang$html$Html$node('dl');
var _elm_lang$html$Html$dt = _elm_lang$html$Html$node('dt');
var _elm_lang$html$Html$dd = _elm_lang$html$Html$node('dd');
var _elm_lang$html$Html$figure = _elm_lang$html$Html$node('figure');
var _elm_lang$html$Html$figcaption = _elm_lang$html$Html$node('figcaption');
var _elm_lang$html$Html$div = _elm_lang$html$Html$node('div');
var _elm_lang$html$Html$a = _elm_lang$html$Html$node('a');
var _elm_lang$html$Html$em = _elm_lang$html$Html$node('em');
var _elm_lang$html$Html$strong = _elm_lang$html$Html$node('strong');
var _elm_lang$html$Html$small = _elm_lang$html$Html$node('small');
var _elm_lang$html$Html$s = _elm_lang$html$Html$node('s');
var _elm_lang$html$Html$cite = _elm_lang$html$Html$node('cite');
var _elm_lang$html$Html$q = _elm_lang$html$Html$node('q');
var _elm_lang$html$Html$dfn = _elm_lang$html$Html$node('dfn');
var _elm_lang$html$Html$abbr = _elm_lang$html$Html$node('abbr');
var _elm_lang$html$Html$time = _elm_lang$html$Html$node('time');
var _elm_lang$html$Html$code = _elm_lang$html$Html$node('code');
var _elm_lang$html$Html$var = _elm_lang$html$Html$node('var');
var _elm_lang$html$Html$samp = _elm_lang$html$Html$node('samp');
var _elm_lang$html$Html$kbd = _elm_lang$html$Html$node('kbd');
var _elm_lang$html$Html$sub = _elm_lang$html$Html$node('sub');
var _elm_lang$html$Html$sup = _elm_lang$html$Html$node('sup');
var _elm_lang$html$Html$i = _elm_lang$html$Html$node('i');
var _elm_lang$html$Html$b = _elm_lang$html$Html$node('b');
var _elm_lang$html$Html$u = _elm_lang$html$Html$node('u');
var _elm_lang$html$Html$mark = _elm_lang$html$Html$node('mark');
var _elm_lang$html$Html$ruby = _elm_lang$html$Html$node('ruby');
var _elm_lang$html$Html$rt = _elm_lang$html$Html$node('rt');
var _elm_lang$html$Html$rp = _elm_lang$html$Html$node('rp');
var _elm_lang$html$Html$bdi = _elm_lang$html$Html$node('bdi');
var _elm_lang$html$Html$bdo = _elm_lang$html$Html$node('bdo');
var _elm_lang$html$Html$span = _elm_lang$html$Html$node('span');
var _elm_lang$html$Html$br = _elm_lang$html$Html$node('br');
var _elm_lang$html$Html$wbr = _elm_lang$html$Html$node('wbr');
var _elm_lang$html$Html$ins = _elm_lang$html$Html$node('ins');
var _elm_lang$html$Html$del = _elm_lang$html$Html$node('del');
var _elm_lang$html$Html$img = _elm_lang$html$Html$node('img');
var _elm_lang$html$Html$iframe = _elm_lang$html$Html$node('iframe');
var _elm_lang$html$Html$embed = _elm_lang$html$Html$node('embed');
var _elm_lang$html$Html$object = _elm_lang$html$Html$node('object');
var _elm_lang$html$Html$param = _elm_lang$html$Html$node('param');
var _elm_lang$html$Html$video = _elm_lang$html$Html$node('video');
var _elm_lang$html$Html$audio = _elm_lang$html$Html$node('audio');
var _elm_lang$html$Html$source = _elm_lang$html$Html$node('source');
var _elm_lang$html$Html$track = _elm_lang$html$Html$node('track');
var _elm_lang$html$Html$canvas = _elm_lang$html$Html$node('canvas');
var _elm_lang$html$Html$math = _elm_lang$html$Html$node('math');
var _elm_lang$html$Html$table = _elm_lang$html$Html$node('table');
var _elm_lang$html$Html$caption = _elm_lang$html$Html$node('caption');
var _elm_lang$html$Html$colgroup = _elm_lang$html$Html$node('colgroup');
var _elm_lang$html$Html$col = _elm_lang$html$Html$node('col');
var _elm_lang$html$Html$tbody = _elm_lang$html$Html$node('tbody');
var _elm_lang$html$Html$thead = _elm_lang$html$Html$node('thead');
var _elm_lang$html$Html$tfoot = _elm_lang$html$Html$node('tfoot');
var _elm_lang$html$Html$tr = _elm_lang$html$Html$node('tr');
var _elm_lang$html$Html$td = _elm_lang$html$Html$node('td');
var _elm_lang$html$Html$th = _elm_lang$html$Html$node('th');
var _elm_lang$html$Html$form = _elm_lang$html$Html$node('form');
var _elm_lang$html$Html$fieldset = _elm_lang$html$Html$node('fieldset');
var _elm_lang$html$Html$legend = _elm_lang$html$Html$node('legend');
var _elm_lang$html$Html$label = _elm_lang$html$Html$node('label');
var _elm_lang$html$Html$input = _elm_lang$html$Html$node('input');
var _elm_lang$html$Html$button = _elm_lang$html$Html$node('button');
var _elm_lang$html$Html$select = _elm_lang$html$Html$node('select');
var _elm_lang$html$Html$datalist = _elm_lang$html$Html$node('datalist');
var _elm_lang$html$Html$optgroup = _elm_lang$html$Html$node('optgroup');
var _elm_lang$html$Html$option = _elm_lang$html$Html$node('option');
var _elm_lang$html$Html$textarea = _elm_lang$html$Html$node('textarea');
var _elm_lang$html$Html$keygen = _elm_lang$html$Html$node('keygen');
var _elm_lang$html$Html$output = _elm_lang$html$Html$node('output');
var _elm_lang$html$Html$progress = _elm_lang$html$Html$node('progress');
var _elm_lang$html$Html$meter = _elm_lang$html$Html$node('meter');
var _elm_lang$html$Html$details = _elm_lang$html$Html$node('details');
var _elm_lang$html$Html$summary = _elm_lang$html$Html$node('summary');
var _elm_lang$html$Html$menuitem = _elm_lang$html$Html$node('menuitem');
var _elm_lang$html$Html$menu = _elm_lang$html$Html$node('menu');

var _elm_lang$html$Html_Attributes$map = _elm_lang$virtual_dom$VirtualDom$mapProperty;
var _elm_lang$html$Html_Attributes$attribute = _elm_lang$virtual_dom$VirtualDom$attribute;
var _elm_lang$html$Html_Attributes$contextmenu = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'contextmenu', value);
};
var _elm_lang$html$Html_Attributes$draggable = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'draggable', value);
};
var _elm_lang$html$Html_Attributes$itemprop = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'itemprop', value);
};
var _elm_lang$html$Html_Attributes$tabindex = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'tabIndex',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$charset = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'charset', value);
};
var _elm_lang$html$Html_Attributes$height = function (value) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'height',
		_elm_lang$core$Basics$toString(value));
};
var _elm_lang$html$Html_Attributes$width = function (value) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'width',
		_elm_lang$core$Basics$toString(value));
};
var _elm_lang$html$Html_Attributes$formaction = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'formAction', value);
};
var _elm_lang$html$Html_Attributes$list = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'list', value);
};
var _elm_lang$html$Html_Attributes$minlength = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'minLength',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$maxlength = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'maxlength',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$size = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'size',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$form = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'form', value);
};
var _elm_lang$html$Html_Attributes$cols = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'cols',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$rows = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'rows',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$challenge = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'challenge', value);
};
var _elm_lang$html$Html_Attributes$media = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'media', value);
};
var _elm_lang$html$Html_Attributes$rel = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'rel', value);
};
var _elm_lang$html$Html_Attributes$datetime = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'datetime', value);
};
var _elm_lang$html$Html_Attributes$pubdate = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'pubdate', value);
};
var _elm_lang$html$Html_Attributes$colspan = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'colspan',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$rowspan = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'rowspan',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$manifest = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'manifest', value);
};
var _elm_lang$html$Html_Attributes$property = _elm_lang$virtual_dom$VirtualDom$property;
var _elm_lang$html$Html_Attributes$stringProperty = F2(
	function (name, string) {
		return A2(
			_elm_lang$html$Html_Attributes$property,
			name,
			_elm_lang$core$Json_Encode$string(string));
	});
var _elm_lang$html$Html_Attributes$class = function (name) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'className', name);
};
var _elm_lang$html$Html_Attributes$id = function (name) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'id', name);
};
var _elm_lang$html$Html_Attributes$title = function (name) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'title', name);
};
var _elm_lang$html$Html_Attributes$accesskey = function ($char) {
	return A2(
		_elm_lang$html$Html_Attributes$stringProperty,
		'accessKey',
		_elm_lang$core$String$fromChar($char));
};
var _elm_lang$html$Html_Attributes$dir = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'dir', value);
};
var _elm_lang$html$Html_Attributes$dropzone = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'dropzone', value);
};
var _elm_lang$html$Html_Attributes$lang = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'lang', value);
};
var _elm_lang$html$Html_Attributes$content = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'content', value);
};
var _elm_lang$html$Html_Attributes$httpEquiv = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'httpEquiv', value);
};
var _elm_lang$html$Html_Attributes$language = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'language', value);
};
var _elm_lang$html$Html_Attributes$src = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'src', value);
};
var _elm_lang$html$Html_Attributes$alt = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'alt', value);
};
var _elm_lang$html$Html_Attributes$preload = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'preload', value);
};
var _elm_lang$html$Html_Attributes$poster = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'poster', value);
};
var _elm_lang$html$Html_Attributes$kind = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'kind', value);
};
var _elm_lang$html$Html_Attributes$srclang = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'srclang', value);
};
var _elm_lang$html$Html_Attributes$sandbox = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'sandbox', value);
};
var _elm_lang$html$Html_Attributes$srcdoc = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'srcdoc', value);
};
var _elm_lang$html$Html_Attributes$type_ = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'type', value);
};
var _elm_lang$html$Html_Attributes$value = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'value', value);
};
var _elm_lang$html$Html_Attributes$defaultValue = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'defaultValue', value);
};
var _elm_lang$html$Html_Attributes$placeholder = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'placeholder', value);
};
var _elm_lang$html$Html_Attributes$accept = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'accept', value);
};
var _elm_lang$html$Html_Attributes$acceptCharset = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'acceptCharset', value);
};
var _elm_lang$html$Html_Attributes$action = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'action', value);
};
var _elm_lang$html$Html_Attributes$autocomplete = function (bool) {
	return A2(
		_elm_lang$html$Html_Attributes$stringProperty,
		'autocomplete',
		bool ? 'on' : 'off');
};
var _elm_lang$html$Html_Attributes$enctype = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'enctype', value);
};
var _elm_lang$html$Html_Attributes$method = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'method', value);
};
var _elm_lang$html$Html_Attributes$name = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'name', value);
};
var _elm_lang$html$Html_Attributes$pattern = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'pattern', value);
};
var _elm_lang$html$Html_Attributes$for = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'htmlFor', value);
};
var _elm_lang$html$Html_Attributes$max = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'max', value);
};
var _elm_lang$html$Html_Attributes$min = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'min', value);
};
var _elm_lang$html$Html_Attributes$step = function (n) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'step', n);
};
var _elm_lang$html$Html_Attributes$wrap = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'wrap', value);
};
var _elm_lang$html$Html_Attributes$usemap = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'useMap', value);
};
var _elm_lang$html$Html_Attributes$shape = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'shape', value);
};
var _elm_lang$html$Html_Attributes$coords = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'coords', value);
};
var _elm_lang$html$Html_Attributes$keytype = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'keytype', value);
};
var _elm_lang$html$Html_Attributes$align = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'align', value);
};
var _elm_lang$html$Html_Attributes$cite = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'cite', value);
};
var _elm_lang$html$Html_Attributes$href = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'href', value);
};
var _elm_lang$html$Html_Attributes$target = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'target', value);
};
var _elm_lang$html$Html_Attributes$downloadAs = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'download', value);
};
var _elm_lang$html$Html_Attributes$hreflang = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'hreflang', value);
};
var _elm_lang$html$Html_Attributes$ping = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'ping', value);
};
var _elm_lang$html$Html_Attributes$start = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$stringProperty,
		'start',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$headers = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'headers', value);
};
var _elm_lang$html$Html_Attributes$scope = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'scope', value);
};
var _elm_lang$html$Html_Attributes$boolProperty = F2(
	function (name, bool) {
		return A2(
			_elm_lang$html$Html_Attributes$property,
			name,
			_elm_lang$core$Json_Encode$bool(bool));
	});
var _elm_lang$html$Html_Attributes$hidden = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'hidden', bool);
};
var _elm_lang$html$Html_Attributes$contenteditable = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'contentEditable', bool);
};
var _elm_lang$html$Html_Attributes$spellcheck = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'spellcheck', bool);
};
var _elm_lang$html$Html_Attributes$async = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'async', bool);
};
var _elm_lang$html$Html_Attributes$defer = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'defer', bool);
};
var _elm_lang$html$Html_Attributes$scoped = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'scoped', bool);
};
var _elm_lang$html$Html_Attributes$autoplay = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'autoplay', bool);
};
var _elm_lang$html$Html_Attributes$controls = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'controls', bool);
};
var _elm_lang$html$Html_Attributes$loop = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'loop', bool);
};
var _elm_lang$html$Html_Attributes$default = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'default', bool);
};
var _elm_lang$html$Html_Attributes$seamless = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'seamless', bool);
};
var _elm_lang$html$Html_Attributes$checked = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'checked', bool);
};
var _elm_lang$html$Html_Attributes$selected = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'selected', bool);
};
var _elm_lang$html$Html_Attributes$autofocus = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'autofocus', bool);
};
var _elm_lang$html$Html_Attributes$disabled = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'disabled', bool);
};
var _elm_lang$html$Html_Attributes$multiple = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'multiple', bool);
};
var _elm_lang$html$Html_Attributes$novalidate = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'noValidate', bool);
};
var _elm_lang$html$Html_Attributes$readonly = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'readOnly', bool);
};
var _elm_lang$html$Html_Attributes$required = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'required', bool);
};
var _elm_lang$html$Html_Attributes$ismap = function (value) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'isMap', value);
};
var _elm_lang$html$Html_Attributes$download = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'download', bool);
};
var _elm_lang$html$Html_Attributes$reversed = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'reversed', bool);
};
var _elm_lang$html$Html_Attributes$classList = function (list) {
	return _elm_lang$html$Html_Attributes$class(
		A2(
			_elm_lang$core$String$join,
			' ',
			A2(
				_elm_lang$core$List$map,
				_elm_lang$core$Tuple$first,
				A2(_elm_lang$core$List$filter, _elm_lang$core$Tuple$second, list))));
};
var _elm_lang$html$Html_Attributes$style = _elm_lang$virtual_dom$VirtualDom$style;

var _elm_lang$html$Html_Events$keyCode = A2(_elm_lang$core$Json_Decode$field, 'keyCode', _elm_lang$core$Json_Decode$int);
var _elm_lang$html$Html_Events$targetChecked = A2(
	_elm_lang$core$Json_Decode$at,
	{
		ctor: '::',
		_0: 'target',
		_1: {
			ctor: '::',
			_0: 'checked',
			_1: {ctor: '[]'}
		}
	},
	_elm_lang$core$Json_Decode$bool);
var _elm_lang$html$Html_Events$targetValue = A2(
	_elm_lang$core$Json_Decode$at,
	{
		ctor: '::',
		_0: 'target',
		_1: {
			ctor: '::',
			_0: 'value',
			_1: {ctor: '[]'}
		}
	},
	_elm_lang$core$Json_Decode$string);
var _elm_lang$html$Html_Events$defaultOptions = _elm_lang$virtual_dom$VirtualDom$defaultOptions;
var _elm_lang$html$Html_Events$onWithOptions = _elm_lang$virtual_dom$VirtualDom$onWithOptions;
var _elm_lang$html$Html_Events$on = _elm_lang$virtual_dom$VirtualDom$on;
var _elm_lang$html$Html_Events$onFocus = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'focus',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onBlur = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'blur',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onSubmitOptions = _elm_lang$core$Native_Utils.update(
	_elm_lang$html$Html_Events$defaultOptions,
	{preventDefault: true});
var _elm_lang$html$Html_Events$onSubmit = function (msg) {
	return A3(
		_elm_lang$html$Html_Events$onWithOptions,
		'submit',
		_elm_lang$html$Html_Events$onSubmitOptions,
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onCheck = function (tagger) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'change',
		A2(_elm_lang$core$Json_Decode$map, tagger, _elm_lang$html$Html_Events$targetChecked));
};
var _elm_lang$html$Html_Events$onInput = function (tagger) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'input',
		A2(_elm_lang$core$Json_Decode$map, tagger, _elm_lang$html$Html_Events$targetValue));
};
var _elm_lang$html$Html_Events$onMouseOut = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseout',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseOver = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseover',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseLeave = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseleave',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseEnter = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseenter',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseUp = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseup',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseDown = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mousedown',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onDoubleClick = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'dblclick',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onClick = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'click',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$Options = F2(
	function (a, b) {
		return {stopPropagation: a, preventDefault: b};
	});

var _elm_lang$keyboard$Keyboard$onSelfMsg = F3(
	function (router, _p0, state) {
		var _p1 = _p0;
		var _p2 = A2(_elm_lang$core$Dict$get, _p1.category, state);
		if (_p2.ctor === 'Nothing') {
			return _elm_lang$core$Task$succeed(state);
		} else {
			var send = function (tagger) {
				return A2(
					_elm_lang$core$Platform$sendToApp,
					router,
					tagger(_p1.keyCode));
			};
			return A2(
				_elm_lang$core$Task$andThen,
				function (_p3) {
					return _elm_lang$core$Task$succeed(state);
				},
				_elm_lang$core$Task$sequence(
					A2(_elm_lang$core$List$map, send, _p2._0.taggers)));
		}
	});
var _elm_lang$keyboard$Keyboard_ops = _elm_lang$keyboard$Keyboard_ops || {};
_elm_lang$keyboard$Keyboard_ops['&>'] = F2(
	function (task1, task2) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (_p4) {
				return task2;
			},
			task1);
	});
var _elm_lang$keyboard$Keyboard$init = _elm_lang$core$Task$succeed(_elm_lang$core$Dict$empty);
var _elm_lang$keyboard$Keyboard$categorizeHelpHelp = F2(
	function (value, maybeValues) {
		var _p5 = maybeValues;
		if (_p5.ctor === 'Nothing') {
			return _elm_lang$core$Maybe$Just(
				{
					ctor: '::',
					_0: value,
					_1: {ctor: '[]'}
				});
		} else {
			return _elm_lang$core$Maybe$Just(
				{ctor: '::', _0: value, _1: _p5._0});
		}
	});
var _elm_lang$keyboard$Keyboard$categorizeHelp = F2(
	function (subs, subDict) {
		categorizeHelp:
		while (true) {
			var _p6 = subs;
			if (_p6.ctor === '[]') {
				return subDict;
			} else {
				var _v4 = _p6._1,
					_v5 = A3(
					_elm_lang$core$Dict$update,
					_p6._0._0,
					_elm_lang$keyboard$Keyboard$categorizeHelpHelp(_p6._0._1),
					subDict);
				subs = _v4;
				subDict = _v5;
				continue categorizeHelp;
			}
		}
	});
var _elm_lang$keyboard$Keyboard$categorize = function (subs) {
	return A2(_elm_lang$keyboard$Keyboard$categorizeHelp, subs, _elm_lang$core$Dict$empty);
};
var _elm_lang$keyboard$Keyboard$keyCode = A2(_elm_lang$core$Json_Decode$field, 'keyCode', _elm_lang$core$Json_Decode$int);
var _elm_lang$keyboard$Keyboard$subscription = _elm_lang$core$Native_Platform.leaf('Keyboard');
var _elm_lang$keyboard$Keyboard$Watcher = F2(
	function (a, b) {
		return {taggers: a, pid: b};
	});
var _elm_lang$keyboard$Keyboard$Msg = F2(
	function (a, b) {
		return {category: a, keyCode: b};
	});
var _elm_lang$keyboard$Keyboard$onEffects = F3(
	function (router, newSubs, oldState) {
		var rightStep = F3(
			function (category, taggers, task) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (state) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (pid) {
								return _elm_lang$core$Task$succeed(
									A3(
										_elm_lang$core$Dict$insert,
										category,
										A2(_elm_lang$keyboard$Keyboard$Watcher, taggers, pid),
										state));
							},
							_elm_lang$core$Process$spawn(
								A3(
									_elm_lang$dom$Dom_LowLevel$onDocument,
									category,
									_elm_lang$keyboard$Keyboard$keyCode,
									function (_p7) {
										return A2(
											_elm_lang$core$Platform$sendToSelf,
											router,
											A2(_elm_lang$keyboard$Keyboard$Msg, category, _p7));
									})));
					},
					task);
			});
		var bothStep = F4(
			function (category, _p8, taggers, task) {
				var _p9 = _p8;
				return A2(
					_elm_lang$core$Task$map,
					A2(
						_elm_lang$core$Dict$insert,
						category,
						A2(_elm_lang$keyboard$Keyboard$Watcher, taggers, _p9.pid)),
					task);
			});
		var leftStep = F3(
			function (category, _p10, task) {
				var _p11 = _p10;
				return A2(
					_elm_lang$keyboard$Keyboard_ops['&>'],
					_elm_lang$core$Process$kill(_p11.pid),
					task);
			});
		return A6(
			_elm_lang$core$Dict$merge,
			leftStep,
			bothStep,
			rightStep,
			oldState,
			_elm_lang$keyboard$Keyboard$categorize(newSubs),
			_elm_lang$core$Task$succeed(_elm_lang$core$Dict$empty));
	});
var _elm_lang$keyboard$Keyboard$MySub = F2(
	function (a, b) {
		return {ctor: 'MySub', _0: a, _1: b};
	});
var _elm_lang$keyboard$Keyboard$presses = function (tagger) {
	return _elm_lang$keyboard$Keyboard$subscription(
		A2(_elm_lang$keyboard$Keyboard$MySub, 'keypress', tagger));
};
var _elm_lang$keyboard$Keyboard$downs = function (tagger) {
	return _elm_lang$keyboard$Keyboard$subscription(
		A2(_elm_lang$keyboard$Keyboard$MySub, 'keydown', tagger));
};
var _elm_lang$keyboard$Keyboard$ups = function (tagger) {
	return _elm_lang$keyboard$Keyboard$subscription(
		A2(_elm_lang$keyboard$Keyboard$MySub, 'keyup', tagger));
};
var _elm_lang$keyboard$Keyboard$subMap = F2(
	function (func, _p12) {
		var _p13 = _p12;
		return A2(
			_elm_lang$keyboard$Keyboard$MySub,
			_p13._0,
			function (_p14) {
				return func(
					_p13._1(_p14));
			});
	});
_elm_lang$core$Native_Platform.effectManagers['Keyboard'] = {pkg: 'elm-lang/keyboard', init: _elm_lang$keyboard$Keyboard$init, onEffects: _elm_lang$keyboard$Keyboard$onEffects, onSelfMsg: _elm_lang$keyboard$Keyboard$onSelfMsg, tag: 'sub', subMap: _elm_lang$keyboard$Keyboard$subMap};

var _elm_lang$mouse$Mouse_ops = _elm_lang$mouse$Mouse_ops || {};
_elm_lang$mouse$Mouse_ops['&>'] = F2(
	function (t1, t2) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (_p0) {
				return t2;
			},
			t1);
	});
var _elm_lang$mouse$Mouse$onSelfMsg = F3(
	function (router, _p1, state) {
		var _p2 = _p1;
		var _p3 = A2(_elm_lang$core$Dict$get, _p2.category, state);
		if (_p3.ctor === 'Nothing') {
			return _elm_lang$core$Task$succeed(state);
		} else {
			var send = function (tagger) {
				return A2(
					_elm_lang$core$Platform$sendToApp,
					router,
					tagger(_p2.position));
			};
			return A2(
				_elm_lang$mouse$Mouse_ops['&>'],
				_elm_lang$core$Task$sequence(
					A2(_elm_lang$core$List$map, send, _p3._0.taggers)),
				_elm_lang$core$Task$succeed(state));
		}
	});
var _elm_lang$mouse$Mouse$init = _elm_lang$core$Task$succeed(_elm_lang$core$Dict$empty);
var _elm_lang$mouse$Mouse$categorizeHelpHelp = F2(
	function (value, maybeValues) {
		var _p4 = maybeValues;
		if (_p4.ctor === 'Nothing') {
			return _elm_lang$core$Maybe$Just(
				{
					ctor: '::',
					_0: value,
					_1: {ctor: '[]'}
				});
		} else {
			return _elm_lang$core$Maybe$Just(
				{ctor: '::', _0: value, _1: _p4._0});
		}
	});
var _elm_lang$mouse$Mouse$categorizeHelp = F2(
	function (subs, subDict) {
		categorizeHelp:
		while (true) {
			var _p5 = subs;
			if (_p5.ctor === '[]') {
				return subDict;
			} else {
				var _v4 = _p5._1,
					_v5 = A3(
					_elm_lang$core$Dict$update,
					_p5._0._0,
					_elm_lang$mouse$Mouse$categorizeHelpHelp(_p5._0._1),
					subDict);
				subs = _v4;
				subDict = _v5;
				continue categorizeHelp;
			}
		}
	});
var _elm_lang$mouse$Mouse$categorize = function (subs) {
	return A2(_elm_lang$mouse$Mouse$categorizeHelp, subs, _elm_lang$core$Dict$empty);
};
var _elm_lang$mouse$Mouse$subscription = _elm_lang$core$Native_Platform.leaf('Mouse');
var _elm_lang$mouse$Mouse$Position = F2(
	function (a, b) {
		return {x: a, y: b};
	});
var _elm_lang$mouse$Mouse$position = A3(
	_elm_lang$core$Json_Decode$map2,
	_elm_lang$mouse$Mouse$Position,
	A2(_elm_lang$core$Json_Decode$field, 'pageX', _elm_lang$core$Json_Decode$int),
	A2(_elm_lang$core$Json_Decode$field, 'pageY', _elm_lang$core$Json_Decode$int));
var _elm_lang$mouse$Mouse$Watcher = F2(
	function (a, b) {
		return {taggers: a, pid: b};
	});
var _elm_lang$mouse$Mouse$Msg = F2(
	function (a, b) {
		return {category: a, position: b};
	});
var _elm_lang$mouse$Mouse$onEffects = F3(
	function (router, newSubs, oldState) {
		var rightStep = F3(
			function (category, taggers, task) {
				var tracker = A3(
					_elm_lang$dom$Dom_LowLevel$onDocument,
					category,
					_elm_lang$mouse$Mouse$position,
					function (_p6) {
						return A2(
							_elm_lang$core$Platform$sendToSelf,
							router,
							A2(_elm_lang$mouse$Mouse$Msg, category, _p6));
					});
				return A2(
					_elm_lang$core$Task$andThen,
					function (state) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (pid) {
								return _elm_lang$core$Task$succeed(
									A3(
										_elm_lang$core$Dict$insert,
										category,
										A2(_elm_lang$mouse$Mouse$Watcher, taggers, pid),
										state));
							},
							_elm_lang$core$Process$spawn(tracker));
					},
					task);
			});
		var bothStep = F4(
			function (category, _p7, taggers, task) {
				var _p8 = _p7;
				return A2(
					_elm_lang$core$Task$andThen,
					function (state) {
						return _elm_lang$core$Task$succeed(
							A3(
								_elm_lang$core$Dict$insert,
								category,
								A2(_elm_lang$mouse$Mouse$Watcher, taggers, _p8.pid),
								state));
					},
					task);
			});
		var leftStep = F3(
			function (category, _p9, task) {
				var _p10 = _p9;
				return A2(
					_elm_lang$mouse$Mouse_ops['&>'],
					_elm_lang$core$Process$kill(_p10.pid),
					task);
			});
		return A6(
			_elm_lang$core$Dict$merge,
			leftStep,
			bothStep,
			rightStep,
			oldState,
			_elm_lang$mouse$Mouse$categorize(newSubs),
			_elm_lang$core$Task$succeed(_elm_lang$core$Dict$empty));
	});
var _elm_lang$mouse$Mouse$MySub = F2(
	function (a, b) {
		return {ctor: 'MySub', _0: a, _1: b};
	});
var _elm_lang$mouse$Mouse$clicks = function (tagger) {
	return _elm_lang$mouse$Mouse$subscription(
		A2(_elm_lang$mouse$Mouse$MySub, 'click', tagger));
};
var _elm_lang$mouse$Mouse$moves = function (tagger) {
	return _elm_lang$mouse$Mouse$subscription(
		A2(_elm_lang$mouse$Mouse$MySub, 'mousemove', tagger));
};
var _elm_lang$mouse$Mouse$downs = function (tagger) {
	return _elm_lang$mouse$Mouse$subscription(
		A2(_elm_lang$mouse$Mouse$MySub, 'mousedown', tagger));
};
var _elm_lang$mouse$Mouse$ups = function (tagger) {
	return _elm_lang$mouse$Mouse$subscription(
		A2(_elm_lang$mouse$Mouse$MySub, 'mouseup', tagger));
};
var _elm_lang$mouse$Mouse$subMap = F2(
	function (func, _p11) {
		var _p12 = _p11;
		return A2(
			_elm_lang$mouse$Mouse$MySub,
			_p12._0,
			function (_p13) {
				return func(
					_p12._1(_p13));
			});
	});
_elm_lang$core$Native_Platform.effectManagers['Mouse'] = {pkg: 'elm-lang/mouse', init: _elm_lang$mouse$Mouse$init, onEffects: _elm_lang$mouse$Mouse$onEffects, onSelfMsg: _elm_lang$mouse$Mouse$onSelfMsg, tag: 'sub', subMap: _elm_lang$mouse$Mouse$subMap};

var _elm_lang$svg$Svg$map = _elm_lang$virtual_dom$VirtualDom$map;
var _elm_lang$svg$Svg$text = _elm_lang$virtual_dom$VirtualDom$text;
var _elm_lang$svg$Svg$svgNamespace = A2(
	_elm_lang$virtual_dom$VirtualDom$property,
	'namespace',
	_elm_lang$core$Json_Encode$string('http://www.w3.org/2000/svg'));
var _elm_lang$svg$Svg$node = F3(
	function (name, attributes, children) {
		return A3(
			_elm_lang$virtual_dom$VirtualDom$node,
			name,
			{ctor: '::', _0: _elm_lang$svg$Svg$svgNamespace, _1: attributes},
			children);
	});
var _elm_lang$svg$Svg$svg = _elm_lang$svg$Svg$node('svg');
var _elm_lang$svg$Svg$foreignObject = _elm_lang$svg$Svg$node('foreignObject');
var _elm_lang$svg$Svg$animate = _elm_lang$svg$Svg$node('animate');
var _elm_lang$svg$Svg$animateColor = _elm_lang$svg$Svg$node('animateColor');
var _elm_lang$svg$Svg$animateMotion = _elm_lang$svg$Svg$node('animateMotion');
var _elm_lang$svg$Svg$animateTransform = _elm_lang$svg$Svg$node('animateTransform');
var _elm_lang$svg$Svg$mpath = _elm_lang$svg$Svg$node('mpath');
var _elm_lang$svg$Svg$set = _elm_lang$svg$Svg$node('set');
var _elm_lang$svg$Svg$a = _elm_lang$svg$Svg$node('a');
var _elm_lang$svg$Svg$defs = _elm_lang$svg$Svg$node('defs');
var _elm_lang$svg$Svg$g = _elm_lang$svg$Svg$node('g');
var _elm_lang$svg$Svg$marker = _elm_lang$svg$Svg$node('marker');
var _elm_lang$svg$Svg$mask = _elm_lang$svg$Svg$node('mask');
var _elm_lang$svg$Svg$pattern = _elm_lang$svg$Svg$node('pattern');
var _elm_lang$svg$Svg$switch = _elm_lang$svg$Svg$node('switch');
var _elm_lang$svg$Svg$symbol = _elm_lang$svg$Svg$node('symbol');
var _elm_lang$svg$Svg$desc = _elm_lang$svg$Svg$node('desc');
var _elm_lang$svg$Svg$metadata = _elm_lang$svg$Svg$node('metadata');
var _elm_lang$svg$Svg$title = _elm_lang$svg$Svg$node('title');
var _elm_lang$svg$Svg$feBlend = _elm_lang$svg$Svg$node('feBlend');
var _elm_lang$svg$Svg$feColorMatrix = _elm_lang$svg$Svg$node('feColorMatrix');
var _elm_lang$svg$Svg$feComponentTransfer = _elm_lang$svg$Svg$node('feComponentTransfer');
var _elm_lang$svg$Svg$feComposite = _elm_lang$svg$Svg$node('feComposite');
var _elm_lang$svg$Svg$feConvolveMatrix = _elm_lang$svg$Svg$node('feConvolveMatrix');
var _elm_lang$svg$Svg$feDiffuseLighting = _elm_lang$svg$Svg$node('feDiffuseLighting');
var _elm_lang$svg$Svg$feDisplacementMap = _elm_lang$svg$Svg$node('feDisplacementMap');
var _elm_lang$svg$Svg$feFlood = _elm_lang$svg$Svg$node('feFlood');
var _elm_lang$svg$Svg$feFuncA = _elm_lang$svg$Svg$node('feFuncA');
var _elm_lang$svg$Svg$feFuncB = _elm_lang$svg$Svg$node('feFuncB');
var _elm_lang$svg$Svg$feFuncG = _elm_lang$svg$Svg$node('feFuncG');
var _elm_lang$svg$Svg$feFuncR = _elm_lang$svg$Svg$node('feFuncR');
var _elm_lang$svg$Svg$feGaussianBlur = _elm_lang$svg$Svg$node('feGaussianBlur');
var _elm_lang$svg$Svg$feImage = _elm_lang$svg$Svg$node('feImage');
var _elm_lang$svg$Svg$feMerge = _elm_lang$svg$Svg$node('feMerge');
var _elm_lang$svg$Svg$feMergeNode = _elm_lang$svg$Svg$node('feMergeNode');
var _elm_lang$svg$Svg$feMorphology = _elm_lang$svg$Svg$node('feMorphology');
var _elm_lang$svg$Svg$feOffset = _elm_lang$svg$Svg$node('feOffset');
var _elm_lang$svg$Svg$feSpecularLighting = _elm_lang$svg$Svg$node('feSpecularLighting');
var _elm_lang$svg$Svg$feTile = _elm_lang$svg$Svg$node('feTile');
var _elm_lang$svg$Svg$feTurbulence = _elm_lang$svg$Svg$node('feTurbulence');
var _elm_lang$svg$Svg$font = _elm_lang$svg$Svg$node('font');
var _elm_lang$svg$Svg$linearGradient = _elm_lang$svg$Svg$node('linearGradient');
var _elm_lang$svg$Svg$radialGradient = _elm_lang$svg$Svg$node('radialGradient');
var _elm_lang$svg$Svg$stop = _elm_lang$svg$Svg$node('stop');
var _elm_lang$svg$Svg$circle = _elm_lang$svg$Svg$node('circle');
var _elm_lang$svg$Svg$ellipse = _elm_lang$svg$Svg$node('ellipse');
var _elm_lang$svg$Svg$image = _elm_lang$svg$Svg$node('image');
var _elm_lang$svg$Svg$line = _elm_lang$svg$Svg$node('line');
var _elm_lang$svg$Svg$path = _elm_lang$svg$Svg$node('path');
var _elm_lang$svg$Svg$polygon = _elm_lang$svg$Svg$node('polygon');
var _elm_lang$svg$Svg$polyline = _elm_lang$svg$Svg$node('polyline');
var _elm_lang$svg$Svg$rect = _elm_lang$svg$Svg$node('rect');
var _elm_lang$svg$Svg$use = _elm_lang$svg$Svg$node('use');
var _elm_lang$svg$Svg$feDistantLight = _elm_lang$svg$Svg$node('feDistantLight');
var _elm_lang$svg$Svg$fePointLight = _elm_lang$svg$Svg$node('fePointLight');
var _elm_lang$svg$Svg$feSpotLight = _elm_lang$svg$Svg$node('feSpotLight');
var _elm_lang$svg$Svg$altGlyph = _elm_lang$svg$Svg$node('altGlyph');
var _elm_lang$svg$Svg$altGlyphDef = _elm_lang$svg$Svg$node('altGlyphDef');
var _elm_lang$svg$Svg$altGlyphItem = _elm_lang$svg$Svg$node('altGlyphItem');
var _elm_lang$svg$Svg$glyph = _elm_lang$svg$Svg$node('glyph');
var _elm_lang$svg$Svg$glyphRef = _elm_lang$svg$Svg$node('glyphRef');
var _elm_lang$svg$Svg$textPath = _elm_lang$svg$Svg$node('textPath');
var _elm_lang$svg$Svg$text_ = _elm_lang$svg$Svg$node('text');
var _elm_lang$svg$Svg$tref = _elm_lang$svg$Svg$node('tref');
var _elm_lang$svg$Svg$tspan = _elm_lang$svg$Svg$node('tspan');
var _elm_lang$svg$Svg$clipPath = _elm_lang$svg$Svg$node('clipPath');
var _elm_lang$svg$Svg$colorProfile = _elm_lang$svg$Svg$node('colorProfile');
var _elm_lang$svg$Svg$cursor = _elm_lang$svg$Svg$node('cursor');
var _elm_lang$svg$Svg$filter = _elm_lang$svg$Svg$node('filter');
var _elm_lang$svg$Svg$script = _elm_lang$svg$Svg$node('script');
var _elm_lang$svg$Svg$style = _elm_lang$svg$Svg$node('style');
var _elm_lang$svg$Svg$view = _elm_lang$svg$Svg$node('view');

var _elm_lang$svg$Svg_Attributes$writingMode = _elm_lang$virtual_dom$VirtualDom$attribute('writing-mode');
var _elm_lang$svg$Svg_Attributes$wordSpacing = _elm_lang$virtual_dom$VirtualDom$attribute('word-spacing');
var _elm_lang$svg$Svg_Attributes$visibility = _elm_lang$virtual_dom$VirtualDom$attribute('visibility');
var _elm_lang$svg$Svg_Attributes$unicodeBidi = _elm_lang$virtual_dom$VirtualDom$attribute('unicode-bidi');
var _elm_lang$svg$Svg_Attributes$textRendering = _elm_lang$virtual_dom$VirtualDom$attribute('text-rendering');
var _elm_lang$svg$Svg_Attributes$textDecoration = _elm_lang$virtual_dom$VirtualDom$attribute('text-decoration');
var _elm_lang$svg$Svg_Attributes$textAnchor = _elm_lang$virtual_dom$VirtualDom$attribute('text-anchor');
var _elm_lang$svg$Svg_Attributes$stroke = _elm_lang$virtual_dom$VirtualDom$attribute('stroke');
var _elm_lang$svg$Svg_Attributes$strokeWidth = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-width');
var _elm_lang$svg$Svg_Attributes$strokeOpacity = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-opacity');
var _elm_lang$svg$Svg_Attributes$strokeMiterlimit = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-miterlimit');
var _elm_lang$svg$Svg_Attributes$strokeLinejoin = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-linejoin');
var _elm_lang$svg$Svg_Attributes$strokeLinecap = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-linecap');
var _elm_lang$svg$Svg_Attributes$strokeDashoffset = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-dashoffset');
var _elm_lang$svg$Svg_Attributes$strokeDasharray = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-dasharray');
var _elm_lang$svg$Svg_Attributes$stopOpacity = _elm_lang$virtual_dom$VirtualDom$attribute('stop-opacity');
var _elm_lang$svg$Svg_Attributes$stopColor = _elm_lang$virtual_dom$VirtualDom$attribute('stop-color');
var _elm_lang$svg$Svg_Attributes$shapeRendering = _elm_lang$virtual_dom$VirtualDom$attribute('shape-rendering');
var _elm_lang$svg$Svg_Attributes$pointerEvents = _elm_lang$virtual_dom$VirtualDom$attribute('pointer-events');
var _elm_lang$svg$Svg_Attributes$overflow = _elm_lang$virtual_dom$VirtualDom$attribute('overflow');
var _elm_lang$svg$Svg_Attributes$opacity = _elm_lang$virtual_dom$VirtualDom$attribute('opacity');
var _elm_lang$svg$Svg_Attributes$mask = _elm_lang$virtual_dom$VirtualDom$attribute('mask');
var _elm_lang$svg$Svg_Attributes$markerStart = _elm_lang$virtual_dom$VirtualDom$attribute('marker-start');
var _elm_lang$svg$Svg_Attributes$markerMid = _elm_lang$virtual_dom$VirtualDom$attribute('marker-mid');
var _elm_lang$svg$Svg_Attributes$markerEnd = _elm_lang$virtual_dom$VirtualDom$attribute('marker-end');
var _elm_lang$svg$Svg_Attributes$lightingColor = _elm_lang$virtual_dom$VirtualDom$attribute('lighting-color');
var _elm_lang$svg$Svg_Attributes$letterSpacing = _elm_lang$virtual_dom$VirtualDom$attribute('letter-spacing');
var _elm_lang$svg$Svg_Attributes$kerning = _elm_lang$virtual_dom$VirtualDom$attribute('kerning');
var _elm_lang$svg$Svg_Attributes$imageRendering = _elm_lang$virtual_dom$VirtualDom$attribute('image-rendering');
var _elm_lang$svg$Svg_Attributes$glyphOrientationVertical = _elm_lang$virtual_dom$VirtualDom$attribute('glyph-orientation-vertical');
var _elm_lang$svg$Svg_Attributes$glyphOrientationHorizontal = _elm_lang$virtual_dom$VirtualDom$attribute('glyph-orientation-horizontal');
var _elm_lang$svg$Svg_Attributes$fontWeight = _elm_lang$virtual_dom$VirtualDom$attribute('font-weight');
var _elm_lang$svg$Svg_Attributes$fontVariant = _elm_lang$virtual_dom$VirtualDom$attribute('font-variant');
var _elm_lang$svg$Svg_Attributes$fontStyle = _elm_lang$virtual_dom$VirtualDom$attribute('font-style');
var _elm_lang$svg$Svg_Attributes$fontStretch = _elm_lang$virtual_dom$VirtualDom$attribute('font-stretch');
var _elm_lang$svg$Svg_Attributes$fontSize = _elm_lang$virtual_dom$VirtualDom$attribute('font-size');
var _elm_lang$svg$Svg_Attributes$fontSizeAdjust = _elm_lang$virtual_dom$VirtualDom$attribute('font-size-adjust');
var _elm_lang$svg$Svg_Attributes$fontFamily = _elm_lang$virtual_dom$VirtualDom$attribute('font-family');
var _elm_lang$svg$Svg_Attributes$floodOpacity = _elm_lang$virtual_dom$VirtualDom$attribute('flood-opacity');
var _elm_lang$svg$Svg_Attributes$floodColor = _elm_lang$virtual_dom$VirtualDom$attribute('flood-color');
var _elm_lang$svg$Svg_Attributes$filter = _elm_lang$virtual_dom$VirtualDom$attribute('filter');
var _elm_lang$svg$Svg_Attributes$fill = _elm_lang$virtual_dom$VirtualDom$attribute('fill');
var _elm_lang$svg$Svg_Attributes$fillRule = _elm_lang$virtual_dom$VirtualDom$attribute('fill-rule');
var _elm_lang$svg$Svg_Attributes$fillOpacity = _elm_lang$virtual_dom$VirtualDom$attribute('fill-opacity');
var _elm_lang$svg$Svg_Attributes$enableBackground = _elm_lang$virtual_dom$VirtualDom$attribute('enable-background');
var _elm_lang$svg$Svg_Attributes$dominantBaseline = _elm_lang$virtual_dom$VirtualDom$attribute('dominant-baseline');
var _elm_lang$svg$Svg_Attributes$display = _elm_lang$virtual_dom$VirtualDom$attribute('display');
var _elm_lang$svg$Svg_Attributes$direction = _elm_lang$virtual_dom$VirtualDom$attribute('direction');
var _elm_lang$svg$Svg_Attributes$cursor = _elm_lang$virtual_dom$VirtualDom$attribute('cursor');
var _elm_lang$svg$Svg_Attributes$color = _elm_lang$virtual_dom$VirtualDom$attribute('color');
var _elm_lang$svg$Svg_Attributes$colorRendering = _elm_lang$virtual_dom$VirtualDom$attribute('color-rendering');
var _elm_lang$svg$Svg_Attributes$colorProfile = _elm_lang$virtual_dom$VirtualDom$attribute('color-profile');
var _elm_lang$svg$Svg_Attributes$colorInterpolation = _elm_lang$virtual_dom$VirtualDom$attribute('color-interpolation');
var _elm_lang$svg$Svg_Attributes$colorInterpolationFilters = _elm_lang$virtual_dom$VirtualDom$attribute('color-interpolation-filters');
var _elm_lang$svg$Svg_Attributes$clip = _elm_lang$virtual_dom$VirtualDom$attribute('clip');
var _elm_lang$svg$Svg_Attributes$clipRule = _elm_lang$virtual_dom$VirtualDom$attribute('clip-rule');
var _elm_lang$svg$Svg_Attributes$clipPath = _elm_lang$virtual_dom$VirtualDom$attribute('clip-path');
var _elm_lang$svg$Svg_Attributes$baselineShift = _elm_lang$virtual_dom$VirtualDom$attribute('baseline-shift');
var _elm_lang$svg$Svg_Attributes$alignmentBaseline = _elm_lang$virtual_dom$VirtualDom$attribute('alignment-baseline');
var _elm_lang$svg$Svg_Attributes$zoomAndPan = _elm_lang$virtual_dom$VirtualDom$attribute('zoomAndPan');
var _elm_lang$svg$Svg_Attributes$z = _elm_lang$virtual_dom$VirtualDom$attribute('z');
var _elm_lang$svg$Svg_Attributes$yChannelSelector = _elm_lang$virtual_dom$VirtualDom$attribute('yChannelSelector');
var _elm_lang$svg$Svg_Attributes$y2 = _elm_lang$virtual_dom$VirtualDom$attribute('y2');
var _elm_lang$svg$Svg_Attributes$y1 = _elm_lang$virtual_dom$VirtualDom$attribute('y1');
var _elm_lang$svg$Svg_Attributes$y = _elm_lang$virtual_dom$VirtualDom$attribute('y');
var _elm_lang$svg$Svg_Attributes$xmlSpace = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/XML/1998/namespace', 'xml:space');
var _elm_lang$svg$Svg_Attributes$xmlLang = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/XML/1998/namespace', 'xml:lang');
var _elm_lang$svg$Svg_Attributes$xmlBase = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/XML/1998/namespace', 'xml:base');
var _elm_lang$svg$Svg_Attributes$xlinkType = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:type');
var _elm_lang$svg$Svg_Attributes$xlinkTitle = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:title');
var _elm_lang$svg$Svg_Attributes$xlinkShow = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:show');
var _elm_lang$svg$Svg_Attributes$xlinkRole = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:role');
var _elm_lang$svg$Svg_Attributes$xlinkHref = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:href');
var _elm_lang$svg$Svg_Attributes$xlinkArcrole = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:arcrole');
var _elm_lang$svg$Svg_Attributes$xlinkActuate = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:actuate');
var _elm_lang$svg$Svg_Attributes$xChannelSelector = _elm_lang$virtual_dom$VirtualDom$attribute('xChannelSelector');
var _elm_lang$svg$Svg_Attributes$x2 = _elm_lang$virtual_dom$VirtualDom$attribute('x2');
var _elm_lang$svg$Svg_Attributes$x1 = _elm_lang$virtual_dom$VirtualDom$attribute('x1');
var _elm_lang$svg$Svg_Attributes$xHeight = _elm_lang$virtual_dom$VirtualDom$attribute('x-height');
var _elm_lang$svg$Svg_Attributes$x = _elm_lang$virtual_dom$VirtualDom$attribute('x');
var _elm_lang$svg$Svg_Attributes$widths = _elm_lang$virtual_dom$VirtualDom$attribute('widths');
var _elm_lang$svg$Svg_Attributes$width = _elm_lang$virtual_dom$VirtualDom$attribute('width');
var _elm_lang$svg$Svg_Attributes$viewTarget = _elm_lang$virtual_dom$VirtualDom$attribute('viewTarget');
var _elm_lang$svg$Svg_Attributes$viewBox = _elm_lang$virtual_dom$VirtualDom$attribute('viewBox');
var _elm_lang$svg$Svg_Attributes$vertOriginY = _elm_lang$virtual_dom$VirtualDom$attribute('vert-origin-y');
var _elm_lang$svg$Svg_Attributes$vertOriginX = _elm_lang$virtual_dom$VirtualDom$attribute('vert-origin-x');
var _elm_lang$svg$Svg_Attributes$vertAdvY = _elm_lang$virtual_dom$VirtualDom$attribute('vert-adv-y');
var _elm_lang$svg$Svg_Attributes$version = _elm_lang$virtual_dom$VirtualDom$attribute('version');
var _elm_lang$svg$Svg_Attributes$values = _elm_lang$virtual_dom$VirtualDom$attribute('values');
var _elm_lang$svg$Svg_Attributes$vMathematical = _elm_lang$virtual_dom$VirtualDom$attribute('v-mathematical');
var _elm_lang$svg$Svg_Attributes$vIdeographic = _elm_lang$virtual_dom$VirtualDom$attribute('v-ideographic');
var _elm_lang$svg$Svg_Attributes$vHanging = _elm_lang$virtual_dom$VirtualDom$attribute('v-hanging');
var _elm_lang$svg$Svg_Attributes$vAlphabetic = _elm_lang$virtual_dom$VirtualDom$attribute('v-alphabetic');
var _elm_lang$svg$Svg_Attributes$unitsPerEm = _elm_lang$virtual_dom$VirtualDom$attribute('units-per-em');
var _elm_lang$svg$Svg_Attributes$unicodeRange = _elm_lang$virtual_dom$VirtualDom$attribute('unicode-range');
var _elm_lang$svg$Svg_Attributes$unicode = _elm_lang$virtual_dom$VirtualDom$attribute('unicode');
var _elm_lang$svg$Svg_Attributes$underlineThickness = _elm_lang$virtual_dom$VirtualDom$attribute('underline-thickness');
var _elm_lang$svg$Svg_Attributes$underlinePosition = _elm_lang$virtual_dom$VirtualDom$attribute('underline-position');
var _elm_lang$svg$Svg_Attributes$u2 = _elm_lang$virtual_dom$VirtualDom$attribute('u2');
var _elm_lang$svg$Svg_Attributes$u1 = _elm_lang$virtual_dom$VirtualDom$attribute('u1');
var _elm_lang$svg$Svg_Attributes$type_ = _elm_lang$virtual_dom$VirtualDom$attribute('type');
var _elm_lang$svg$Svg_Attributes$transform = _elm_lang$virtual_dom$VirtualDom$attribute('transform');
var _elm_lang$svg$Svg_Attributes$to = _elm_lang$virtual_dom$VirtualDom$attribute('to');
var _elm_lang$svg$Svg_Attributes$title = _elm_lang$virtual_dom$VirtualDom$attribute('title');
var _elm_lang$svg$Svg_Attributes$textLength = _elm_lang$virtual_dom$VirtualDom$attribute('textLength');
var _elm_lang$svg$Svg_Attributes$targetY = _elm_lang$virtual_dom$VirtualDom$attribute('targetY');
var _elm_lang$svg$Svg_Attributes$targetX = _elm_lang$virtual_dom$VirtualDom$attribute('targetX');
var _elm_lang$svg$Svg_Attributes$target = _elm_lang$virtual_dom$VirtualDom$attribute('target');
var _elm_lang$svg$Svg_Attributes$tableValues = _elm_lang$virtual_dom$VirtualDom$attribute('tableValues');
var _elm_lang$svg$Svg_Attributes$systemLanguage = _elm_lang$virtual_dom$VirtualDom$attribute('systemLanguage');
var _elm_lang$svg$Svg_Attributes$surfaceScale = _elm_lang$virtual_dom$VirtualDom$attribute('surfaceScale');
var _elm_lang$svg$Svg_Attributes$style = _elm_lang$virtual_dom$VirtualDom$attribute('style');
var _elm_lang$svg$Svg_Attributes$string = _elm_lang$virtual_dom$VirtualDom$attribute('string');
var _elm_lang$svg$Svg_Attributes$strikethroughThickness = _elm_lang$virtual_dom$VirtualDom$attribute('strikethrough-thickness');
var _elm_lang$svg$Svg_Attributes$strikethroughPosition = _elm_lang$virtual_dom$VirtualDom$attribute('strikethrough-position');
var _elm_lang$svg$Svg_Attributes$stitchTiles = _elm_lang$virtual_dom$VirtualDom$attribute('stitchTiles');
var _elm_lang$svg$Svg_Attributes$stemv = _elm_lang$virtual_dom$VirtualDom$attribute('stemv');
var _elm_lang$svg$Svg_Attributes$stemh = _elm_lang$virtual_dom$VirtualDom$attribute('stemh');
var _elm_lang$svg$Svg_Attributes$stdDeviation = _elm_lang$virtual_dom$VirtualDom$attribute('stdDeviation');
var _elm_lang$svg$Svg_Attributes$startOffset = _elm_lang$virtual_dom$VirtualDom$attribute('startOffset');
var _elm_lang$svg$Svg_Attributes$spreadMethod = _elm_lang$virtual_dom$VirtualDom$attribute('spreadMethod');
var _elm_lang$svg$Svg_Attributes$speed = _elm_lang$virtual_dom$VirtualDom$attribute('speed');
var _elm_lang$svg$Svg_Attributes$specularExponent = _elm_lang$virtual_dom$VirtualDom$attribute('specularExponent');
var _elm_lang$svg$Svg_Attributes$specularConstant = _elm_lang$virtual_dom$VirtualDom$attribute('specularConstant');
var _elm_lang$svg$Svg_Attributes$spacing = _elm_lang$virtual_dom$VirtualDom$attribute('spacing');
var _elm_lang$svg$Svg_Attributes$slope = _elm_lang$virtual_dom$VirtualDom$attribute('slope');
var _elm_lang$svg$Svg_Attributes$seed = _elm_lang$virtual_dom$VirtualDom$attribute('seed');
var _elm_lang$svg$Svg_Attributes$scale = _elm_lang$virtual_dom$VirtualDom$attribute('scale');
var _elm_lang$svg$Svg_Attributes$ry = _elm_lang$virtual_dom$VirtualDom$attribute('ry');
var _elm_lang$svg$Svg_Attributes$rx = _elm_lang$virtual_dom$VirtualDom$attribute('rx');
var _elm_lang$svg$Svg_Attributes$rotate = _elm_lang$virtual_dom$VirtualDom$attribute('rotate');
var _elm_lang$svg$Svg_Attributes$result = _elm_lang$virtual_dom$VirtualDom$attribute('result');
var _elm_lang$svg$Svg_Attributes$restart = _elm_lang$virtual_dom$VirtualDom$attribute('restart');
var _elm_lang$svg$Svg_Attributes$requiredFeatures = _elm_lang$virtual_dom$VirtualDom$attribute('requiredFeatures');
var _elm_lang$svg$Svg_Attributes$requiredExtensions = _elm_lang$virtual_dom$VirtualDom$attribute('requiredExtensions');
var _elm_lang$svg$Svg_Attributes$repeatDur = _elm_lang$virtual_dom$VirtualDom$attribute('repeatDur');
var _elm_lang$svg$Svg_Attributes$repeatCount = _elm_lang$virtual_dom$VirtualDom$attribute('repeatCount');
var _elm_lang$svg$Svg_Attributes$renderingIntent = _elm_lang$virtual_dom$VirtualDom$attribute('rendering-intent');
var _elm_lang$svg$Svg_Attributes$refY = _elm_lang$virtual_dom$VirtualDom$attribute('refY');
var _elm_lang$svg$Svg_Attributes$refX = _elm_lang$virtual_dom$VirtualDom$attribute('refX');
var _elm_lang$svg$Svg_Attributes$radius = _elm_lang$virtual_dom$VirtualDom$attribute('radius');
var _elm_lang$svg$Svg_Attributes$r = _elm_lang$virtual_dom$VirtualDom$attribute('r');
var _elm_lang$svg$Svg_Attributes$primitiveUnits = _elm_lang$virtual_dom$VirtualDom$attribute('primitiveUnits');
var _elm_lang$svg$Svg_Attributes$preserveAspectRatio = _elm_lang$virtual_dom$VirtualDom$attribute('preserveAspectRatio');
var _elm_lang$svg$Svg_Attributes$preserveAlpha = _elm_lang$virtual_dom$VirtualDom$attribute('preserveAlpha');
var _elm_lang$svg$Svg_Attributes$pointsAtZ = _elm_lang$virtual_dom$VirtualDom$attribute('pointsAtZ');
var _elm_lang$svg$Svg_Attributes$pointsAtY = _elm_lang$virtual_dom$VirtualDom$attribute('pointsAtY');
var _elm_lang$svg$Svg_Attributes$pointsAtX = _elm_lang$virtual_dom$VirtualDom$attribute('pointsAtX');
var _elm_lang$svg$Svg_Attributes$points = _elm_lang$virtual_dom$VirtualDom$attribute('points');
var _elm_lang$svg$Svg_Attributes$pointOrder = _elm_lang$virtual_dom$VirtualDom$attribute('point-order');
var _elm_lang$svg$Svg_Attributes$patternUnits = _elm_lang$virtual_dom$VirtualDom$attribute('patternUnits');
var _elm_lang$svg$Svg_Attributes$patternTransform = _elm_lang$virtual_dom$VirtualDom$attribute('patternTransform');
var _elm_lang$svg$Svg_Attributes$patternContentUnits = _elm_lang$virtual_dom$VirtualDom$attribute('patternContentUnits');
var _elm_lang$svg$Svg_Attributes$pathLength = _elm_lang$virtual_dom$VirtualDom$attribute('pathLength');
var _elm_lang$svg$Svg_Attributes$path = _elm_lang$virtual_dom$VirtualDom$attribute('path');
var _elm_lang$svg$Svg_Attributes$panose1 = _elm_lang$virtual_dom$VirtualDom$attribute('panose-1');
var _elm_lang$svg$Svg_Attributes$overlineThickness = _elm_lang$virtual_dom$VirtualDom$attribute('overline-thickness');
var _elm_lang$svg$Svg_Attributes$overlinePosition = _elm_lang$virtual_dom$VirtualDom$attribute('overline-position');
var _elm_lang$svg$Svg_Attributes$origin = _elm_lang$virtual_dom$VirtualDom$attribute('origin');
var _elm_lang$svg$Svg_Attributes$orientation = _elm_lang$virtual_dom$VirtualDom$attribute('orientation');
var _elm_lang$svg$Svg_Attributes$orient = _elm_lang$virtual_dom$VirtualDom$attribute('orient');
var _elm_lang$svg$Svg_Attributes$order = _elm_lang$virtual_dom$VirtualDom$attribute('order');
var _elm_lang$svg$Svg_Attributes$operator = _elm_lang$virtual_dom$VirtualDom$attribute('operator');
var _elm_lang$svg$Svg_Attributes$offset = _elm_lang$virtual_dom$VirtualDom$attribute('offset');
var _elm_lang$svg$Svg_Attributes$numOctaves = _elm_lang$virtual_dom$VirtualDom$attribute('numOctaves');
var _elm_lang$svg$Svg_Attributes$name = _elm_lang$virtual_dom$VirtualDom$attribute('name');
var _elm_lang$svg$Svg_Attributes$mode = _elm_lang$virtual_dom$VirtualDom$attribute('mode');
var _elm_lang$svg$Svg_Attributes$min = _elm_lang$virtual_dom$VirtualDom$attribute('min');
var _elm_lang$svg$Svg_Attributes$method = _elm_lang$virtual_dom$VirtualDom$attribute('method');
var _elm_lang$svg$Svg_Attributes$media = _elm_lang$virtual_dom$VirtualDom$attribute('media');
var _elm_lang$svg$Svg_Attributes$max = _elm_lang$virtual_dom$VirtualDom$attribute('max');
var _elm_lang$svg$Svg_Attributes$mathematical = _elm_lang$virtual_dom$VirtualDom$attribute('mathematical');
var _elm_lang$svg$Svg_Attributes$maskUnits = _elm_lang$virtual_dom$VirtualDom$attribute('maskUnits');
var _elm_lang$svg$Svg_Attributes$maskContentUnits = _elm_lang$virtual_dom$VirtualDom$attribute('maskContentUnits');
var _elm_lang$svg$Svg_Attributes$markerWidth = _elm_lang$virtual_dom$VirtualDom$attribute('markerWidth');
var _elm_lang$svg$Svg_Attributes$markerUnits = _elm_lang$virtual_dom$VirtualDom$attribute('markerUnits');
var _elm_lang$svg$Svg_Attributes$markerHeight = _elm_lang$virtual_dom$VirtualDom$attribute('markerHeight');
var _elm_lang$svg$Svg_Attributes$local = _elm_lang$virtual_dom$VirtualDom$attribute('local');
var _elm_lang$svg$Svg_Attributes$limitingConeAngle = _elm_lang$virtual_dom$VirtualDom$attribute('limitingConeAngle');
var _elm_lang$svg$Svg_Attributes$lengthAdjust = _elm_lang$virtual_dom$VirtualDom$attribute('lengthAdjust');
var _elm_lang$svg$Svg_Attributes$lang = _elm_lang$virtual_dom$VirtualDom$attribute('lang');
var _elm_lang$svg$Svg_Attributes$keyTimes = _elm_lang$virtual_dom$VirtualDom$attribute('keyTimes');
var _elm_lang$svg$Svg_Attributes$keySplines = _elm_lang$virtual_dom$VirtualDom$attribute('keySplines');
var _elm_lang$svg$Svg_Attributes$keyPoints = _elm_lang$virtual_dom$VirtualDom$attribute('keyPoints');
var _elm_lang$svg$Svg_Attributes$kernelUnitLength = _elm_lang$virtual_dom$VirtualDom$attribute('kernelUnitLength');
var _elm_lang$svg$Svg_Attributes$kernelMatrix = _elm_lang$virtual_dom$VirtualDom$attribute('kernelMatrix');
var _elm_lang$svg$Svg_Attributes$k4 = _elm_lang$virtual_dom$VirtualDom$attribute('k4');
var _elm_lang$svg$Svg_Attributes$k3 = _elm_lang$virtual_dom$VirtualDom$attribute('k3');
var _elm_lang$svg$Svg_Attributes$k2 = _elm_lang$virtual_dom$VirtualDom$attribute('k2');
var _elm_lang$svg$Svg_Attributes$k1 = _elm_lang$virtual_dom$VirtualDom$attribute('k1');
var _elm_lang$svg$Svg_Attributes$k = _elm_lang$virtual_dom$VirtualDom$attribute('k');
var _elm_lang$svg$Svg_Attributes$intercept = _elm_lang$virtual_dom$VirtualDom$attribute('intercept');
var _elm_lang$svg$Svg_Attributes$in2 = _elm_lang$virtual_dom$VirtualDom$attribute('in2');
var _elm_lang$svg$Svg_Attributes$in_ = _elm_lang$virtual_dom$VirtualDom$attribute('in');
var _elm_lang$svg$Svg_Attributes$ideographic = _elm_lang$virtual_dom$VirtualDom$attribute('ideographic');
var _elm_lang$svg$Svg_Attributes$id = _elm_lang$virtual_dom$VirtualDom$attribute('id');
var _elm_lang$svg$Svg_Attributes$horizOriginY = _elm_lang$virtual_dom$VirtualDom$attribute('horiz-origin-y');
var _elm_lang$svg$Svg_Attributes$horizOriginX = _elm_lang$virtual_dom$VirtualDom$attribute('horiz-origin-x');
var _elm_lang$svg$Svg_Attributes$horizAdvX = _elm_lang$virtual_dom$VirtualDom$attribute('horiz-adv-x');
var _elm_lang$svg$Svg_Attributes$height = _elm_lang$virtual_dom$VirtualDom$attribute('height');
var _elm_lang$svg$Svg_Attributes$hanging = _elm_lang$virtual_dom$VirtualDom$attribute('hanging');
var _elm_lang$svg$Svg_Attributes$gradientUnits = _elm_lang$virtual_dom$VirtualDom$attribute('gradientUnits');
var _elm_lang$svg$Svg_Attributes$gradientTransform = _elm_lang$virtual_dom$VirtualDom$attribute('gradientTransform');
var _elm_lang$svg$Svg_Attributes$glyphRef = _elm_lang$virtual_dom$VirtualDom$attribute('glyphRef');
var _elm_lang$svg$Svg_Attributes$glyphName = _elm_lang$virtual_dom$VirtualDom$attribute('glyph-name');
var _elm_lang$svg$Svg_Attributes$g2 = _elm_lang$virtual_dom$VirtualDom$attribute('g2');
var _elm_lang$svg$Svg_Attributes$g1 = _elm_lang$virtual_dom$VirtualDom$attribute('g1');
var _elm_lang$svg$Svg_Attributes$fy = _elm_lang$virtual_dom$VirtualDom$attribute('fy');
var _elm_lang$svg$Svg_Attributes$fx = _elm_lang$virtual_dom$VirtualDom$attribute('fx');
var _elm_lang$svg$Svg_Attributes$from = _elm_lang$virtual_dom$VirtualDom$attribute('from');
var _elm_lang$svg$Svg_Attributes$format = _elm_lang$virtual_dom$VirtualDom$attribute('format');
var _elm_lang$svg$Svg_Attributes$filterUnits = _elm_lang$virtual_dom$VirtualDom$attribute('filterUnits');
var _elm_lang$svg$Svg_Attributes$filterRes = _elm_lang$virtual_dom$VirtualDom$attribute('filterRes');
var _elm_lang$svg$Svg_Attributes$externalResourcesRequired = _elm_lang$virtual_dom$VirtualDom$attribute('externalResourcesRequired');
var _elm_lang$svg$Svg_Attributes$exponent = _elm_lang$virtual_dom$VirtualDom$attribute('exponent');
var _elm_lang$svg$Svg_Attributes$end = _elm_lang$virtual_dom$VirtualDom$attribute('end');
var _elm_lang$svg$Svg_Attributes$elevation = _elm_lang$virtual_dom$VirtualDom$attribute('elevation');
var _elm_lang$svg$Svg_Attributes$edgeMode = _elm_lang$virtual_dom$VirtualDom$attribute('edgeMode');
var _elm_lang$svg$Svg_Attributes$dy = _elm_lang$virtual_dom$VirtualDom$attribute('dy');
var _elm_lang$svg$Svg_Attributes$dx = _elm_lang$virtual_dom$VirtualDom$attribute('dx');
var _elm_lang$svg$Svg_Attributes$dur = _elm_lang$virtual_dom$VirtualDom$attribute('dur');
var _elm_lang$svg$Svg_Attributes$divisor = _elm_lang$virtual_dom$VirtualDom$attribute('divisor');
var _elm_lang$svg$Svg_Attributes$diffuseConstant = _elm_lang$virtual_dom$VirtualDom$attribute('diffuseConstant');
var _elm_lang$svg$Svg_Attributes$descent = _elm_lang$virtual_dom$VirtualDom$attribute('descent');
var _elm_lang$svg$Svg_Attributes$decelerate = _elm_lang$virtual_dom$VirtualDom$attribute('decelerate');
var _elm_lang$svg$Svg_Attributes$d = _elm_lang$virtual_dom$VirtualDom$attribute('d');
var _elm_lang$svg$Svg_Attributes$cy = _elm_lang$virtual_dom$VirtualDom$attribute('cy');
var _elm_lang$svg$Svg_Attributes$cx = _elm_lang$virtual_dom$VirtualDom$attribute('cx');
var _elm_lang$svg$Svg_Attributes$contentStyleType = _elm_lang$virtual_dom$VirtualDom$attribute('contentStyleType');
var _elm_lang$svg$Svg_Attributes$contentScriptType = _elm_lang$virtual_dom$VirtualDom$attribute('contentScriptType');
var _elm_lang$svg$Svg_Attributes$clipPathUnits = _elm_lang$virtual_dom$VirtualDom$attribute('clipPathUnits');
var _elm_lang$svg$Svg_Attributes$class = _elm_lang$virtual_dom$VirtualDom$attribute('class');
var _elm_lang$svg$Svg_Attributes$capHeight = _elm_lang$virtual_dom$VirtualDom$attribute('cap-height');
var _elm_lang$svg$Svg_Attributes$calcMode = _elm_lang$virtual_dom$VirtualDom$attribute('calcMode');
var _elm_lang$svg$Svg_Attributes$by = _elm_lang$virtual_dom$VirtualDom$attribute('by');
var _elm_lang$svg$Svg_Attributes$bias = _elm_lang$virtual_dom$VirtualDom$attribute('bias');
var _elm_lang$svg$Svg_Attributes$begin = _elm_lang$virtual_dom$VirtualDom$attribute('begin');
var _elm_lang$svg$Svg_Attributes$bbox = _elm_lang$virtual_dom$VirtualDom$attribute('bbox');
var _elm_lang$svg$Svg_Attributes$baseProfile = _elm_lang$virtual_dom$VirtualDom$attribute('baseProfile');
var _elm_lang$svg$Svg_Attributes$baseFrequency = _elm_lang$virtual_dom$VirtualDom$attribute('baseFrequency');
var _elm_lang$svg$Svg_Attributes$azimuth = _elm_lang$virtual_dom$VirtualDom$attribute('azimuth');
var _elm_lang$svg$Svg_Attributes$autoReverse = _elm_lang$virtual_dom$VirtualDom$attribute('autoReverse');
var _elm_lang$svg$Svg_Attributes$attributeType = _elm_lang$virtual_dom$VirtualDom$attribute('attributeType');
var _elm_lang$svg$Svg_Attributes$attributeName = _elm_lang$virtual_dom$VirtualDom$attribute('attributeName');
var _elm_lang$svg$Svg_Attributes$ascent = _elm_lang$virtual_dom$VirtualDom$attribute('ascent');
var _elm_lang$svg$Svg_Attributes$arabicForm = _elm_lang$virtual_dom$VirtualDom$attribute('arabic-form');
var _elm_lang$svg$Svg_Attributes$amplitude = _elm_lang$virtual_dom$VirtualDom$attribute('amplitude');
var _elm_lang$svg$Svg_Attributes$allowReorder = _elm_lang$virtual_dom$VirtualDom$attribute('allowReorder');
var _elm_lang$svg$Svg_Attributes$alphabetic = _elm_lang$virtual_dom$VirtualDom$attribute('alphabetic');
var _elm_lang$svg$Svg_Attributes$additive = _elm_lang$virtual_dom$VirtualDom$attribute('additive');
var _elm_lang$svg$Svg_Attributes$accumulate = _elm_lang$virtual_dom$VirtualDom$attribute('accumulate');
var _elm_lang$svg$Svg_Attributes$accelerate = _elm_lang$virtual_dom$VirtualDom$attribute('accelerate');
var _elm_lang$svg$Svg_Attributes$accentHeight = _elm_lang$virtual_dom$VirtualDom$attribute('accent-height');

var _elm_lang$svg$Svg_Events$on = _elm_lang$virtual_dom$VirtualDom$on;
var _elm_lang$svg$Svg_Events$simpleOn = F2(
	function (name, msg) {
		return A2(
			_elm_lang$svg$Svg_Events$on,
			name,
			_elm_lang$core$Json_Decode$succeed(msg));
	});
var _elm_lang$svg$Svg_Events$onBegin = _elm_lang$svg$Svg_Events$simpleOn('begin');
var _elm_lang$svg$Svg_Events$onEnd = _elm_lang$svg$Svg_Events$simpleOn('end');
var _elm_lang$svg$Svg_Events$onRepeat = _elm_lang$svg$Svg_Events$simpleOn('repeat');
var _elm_lang$svg$Svg_Events$onAbort = _elm_lang$svg$Svg_Events$simpleOn('abort');
var _elm_lang$svg$Svg_Events$onError = _elm_lang$svg$Svg_Events$simpleOn('error');
var _elm_lang$svg$Svg_Events$onResize = _elm_lang$svg$Svg_Events$simpleOn('resize');
var _elm_lang$svg$Svg_Events$onScroll = _elm_lang$svg$Svg_Events$simpleOn('scroll');
var _elm_lang$svg$Svg_Events$onLoad = _elm_lang$svg$Svg_Events$simpleOn('load');
var _elm_lang$svg$Svg_Events$onUnload = _elm_lang$svg$Svg_Events$simpleOn('unload');
var _elm_lang$svg$Svg_Events$onZoom = _elm_lang$svg$Svg_Events$simpleOn('zoom');
var _elm_lang$svg$Svg_Events$onActivate = _elm_lang$svg$Svg_Events$simpleOn('activate');
var _elm_lang$svg$Svg_Events$onClick = _elm_lang$svg$Svg_Events$simpleOn('click');
var _elm_lang$svg$Svg_Events$onFocusIn = _elm_lang$svg$Svg_Events$simpleOn('focusin');
var _elm_lang$svg$Svg_Events$onFocusOut = _elm_lang$svg$Svg_Events$simpleOn('focusout');
var _elm_lang$svg$Svg_Events$onMouseDown = _elm_lang$svg$Svg_Events$simpleOn('mousedown');
var _elm_lang$svg$Svg_Events$onMouseMove = _elm_lang$svg$Svg_Events$simpleOn('mousemove');
var _elm_lang$svg$Svg_Events$onMouseOut = _elm_lang$svg$Svg_Events$simpleOn('mouseout');
var _elm_lang$svg$Svg_Events$onMouseOver = _elm_lang$svg$Svg_Events$simpleOn('mouseover');
var _elm_lang$svg$Svg_Events$onMouseUp = _elm_lang$svg$Svg_Events$simpleOn('mouseup');

var _elm_lang$svg$Svg_Keyed$svgNamespace = A2(
	_elm_lang$virtual_dom$VirtualDom$property,
	'namespace',
	_elm_lang$core$Json_Encode$string('http://www.w3.org/2000/svg'));
var _elm_lang$svg$Svg_Keyed$node = F3(
	function (name, attributes, children) {
		return A3(
			_elm_lang$virtual_dom$VirtualDom$keyedNode,
			name,
			{ctor: '::', _0: _elm_lang$svg$Svg_Keyed$svgNamespace, _1: attributes},
			children);
	});

var _elm_lang$window$Native_Window = function()
{

var size = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)	{
	callback(_elm_lang$core$Native_Scheduler.succeed({
		width: window.innerWidth,
		height: window.innerHeight
	}));
});

return {
	size: size
};

}();
var _elm_lang$window$Window_ops = _elm_lang$window$Window_ops || {};
_elm_lang$window$Window_ops['&>'] = F2(
	function (task1, task2) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (_p0) {
				return task2;
			},
			task1);
	});
var _elm_lang$window$Window$onSelfMsg = F3(
	function (router, dimensions, state) {
		var _p1 = state;
		if (_p1.ctor === 'Nothing') {
			return _elm_lang$core$Task$succeed(state);
		} else {
			var send = function (_p2) {
				var _p3 = _p2;
				return A2(
					_elm_lang$core$Platform$sendToApp,
					router,
					_p3._0(dimensions));
			};
			return A2(
				_elm_lang$window$Window_ops['&>'],
				_elm_lang$core$Task$sequence(
					A2(_elm_lang$core$List$map, send, _p1._0.subs)),
				_elm_lang$core$Task$succeed(state));
		}
	});
var _elm_lang$window$Window$init = _elm_lang$core$Task$succeed(_elm_lang$core$Maybe$Nothing);
var _elm_lang$window$Window$size = _elm_lang$window$Native_Window.size;
var _elm_lang$window$Window$width = A2(
	_elm_lang$core$Task$map,
	function (_) {
		return _.width;
	},
	_elm_lang$window$Window$size);
var _elm_lang$window$Window$height = A2(
	_elm_lang$core$Task$map,
	function (_) {
		return _.height;
	},
	_elm_lang$window$Window$size);
var _elm_lang$window$Window$onEffects = F3(
	function (router, newSubs, oldState) {
		var _p4 = {ctor: '_Tuple2', _0: oldState, _1: newSubs};
		if (_p4._0.ctor === 'Nothing') {
			if (_p4._1.ctor === '[]') {
				return _elm_lang$core$Task$succeed(_elm_lang$core$Maybe$Nothing);
			} else {
				return A2(
					_elm_lang$core$Task$andThen,
					function (pid) {
						return _elm_lang$core$Task$succeed(
							_elm_lang$core$Maybe$Just(
								{subs: newSubs, pid: pid}));
					},
					_elm_lang$core$Process$spawn(
						A3(
							_elm_lang$dom$Dom_LowLevel$onWindow,
							'resize',
							_elm_lang$core$Json_Decode$succeed(
								{ctor: '_Tuple0'}),
							function (_p5) {
								return A2(
									_elm_lang$core$Task$andThen,
									_elm_lang$core$Platform$sendToSelf(router),
									_elm_lang$window$Window$size);
							})));
			}
		} else {
			if (_p4._1.ctor === '[]') {
				return A2(
					_elm_lang$window$Window_ops['&>'],
					_elm_lang$core$Process$kill(_p4._0._0.pid),
					_elm_lang$core$Task$succeed(_elm_lang$core$Maybe$Nothing));
			} else {
				return _elm_lang$core$Task$succeed(
					_elm_lang$core$Maybe$Just(
						{subs: newSubs, pid: _p4._0._0.pid}));
			}
		}
	});
var _elm_lang$window$Window$subscription = _elm_lang$core$Native_Platform.leaf('Window');
var _elm_lang$window$Window$Size = F2(
	function (a, b) {
		return {width: a, height: b};
	});
var _elm_lang$window$Window$MySub = function (a) {
	return {ctor: 'MySub', _0: a};
};
var _elm_lang$window$Window$resizes = function (tagger) {
	return _elm_lang$window$Window$subscription(
		_elm_lang$window$Window$MySub(tagger));
};
var _elm_lang$window$Window$subMap = F2(
	function (func, _p6) {
		var _p7 = _p6;
		return _elm_lang$window$Window$MySub(
			function (_p8) {
				return func(
					_p7._0(_p8));
			});
	});
_elm_lang$core$Native_Platform.effectManagers['Window'] = {pkg: 'elm-lang/window', init: _elm_lang$window$Window$init, onEffects: _elm_lang$window$Window$onEffects, onSelfMsg: _elm_lang$window$Window$onSelfMsg, tag: 'sub', subMap: _elm_lang$window$Window$subMap};

var _fredcy$elm_parseint$ParseInt$charFromInt = function (i) {
	return (_elm_lang$core$Native_Utils.cmp(i, 10) < 0) ? _elm_lang$core$Char$fromCode(
		i + _elm_lang$core$Char$toCode(
			_elm_lang$core$Native_Utils.chr('0'))) : ((_elm_lang$core$Native_Utils.cmp(i, 36) < 0) ? _elm_lang$core$Char$fromCode(
		(i - 10) + _elm_lang$core$Char$toCode(
			_elm_lang$core$Native_Utils.chr('A'))) : _elm_lang$core$Native_Utils.crash(
		'ParseInt',
		{
			start: {line: 158, column: 9},
			end: {line: 158, column: 20}
		})(
		_elm_lang$core$Basics$toString(i)));
};
var _fredcy$elm_parseint$ParseInt$toRadixUnsafe = F2(
	function (radix, i) {
		return (_elm_lang$core$Native_Utils.cmp(i, radix) < 0) ? _elm_lang$core$String$fromChar(
			_fredcy$elm_parseint$ParseInt$charFromInt(i)) : A2(
			_elm_lang$core$Basics_ops['++'],
			A2(_fredcy$elm_parseint$ParseInt$toRadixUnsafe, radix, (i / radix) | 0),
			_elm_lang$core$String$fromChar(
				_fredcy$elm_parseint$ParseInt$charFromInt(
					A2(_elm_lang$core$Basics_ops['%'], i, radix))));
	});
var _fredcy$elm_parseint$ParseInt$toOct = _fredcy$elm_parseint$ParseInt$toRadixUnsafe(8);
var _fredcy$elm_parseint$ParseInt$toHex = _fredcy$elm_parseint$ParseInt$toRadixUnsafe(16);
var _fredcy$elm_parseint$ParseInt$isBetween = F3(
	function (lower, upper, c) {
		var ci = _elm_lang$core$Char$toCode(c);
		return (_elm_lang$core$Native_Utils.cmp(
			_elm_lang$core$Char$toCode(lower),
			ci) < 1) && (_elm_lang$core$Native_Utils.cmp(
			ci,
			_elm_lang$core$Char$toCode(upper)) < 1);
	});
var _fredcy$elm_parseint$ParseInt$charOffset = F2(
	function (basis, c) {
		return _elm_lang$core$Char$toCode(c) - _elm_lang$core$Char$toCode(basis);
	});
var _fredcy$elm_parseint$ParseInt$InvalidRadix = function (a) {
	return {ctor: 'InvalidRadix', _0: a};
};
var _fredcy$elm_parseint$ParseInt$toRadix = F2(
	function (radix, i) {
		return ((_elm_lang$core$Native_Utils.cmp(2, radix) < 1) && (_elm_lang$core$Native_Utils.cmp(radix, 36) < 1)) ? ((_elm_lang$core$Native_Utils.cmp(i, 0) < 0) ? _elm_lang$core$Result$Ok(
			A2(
				_elm_lang$core$Basics_ops['++'],
				'-',
				A2(_fredcy$elm_parseint$ParseInt$toRadixUnsafe, radix, 0 - i))) : _elm_lang$core$Result$Ok(
			A2(_fredcy$elm_parseint$ParseInt$toRadixUnsafe, radix, i))) : _elm_lang$core$Result$Err(
			_fredcy$elm_parseint$ParseInt$InvalidRadix(radix));
	});
var _fredcy$elm_parseint$ParseInt$OutOfRange = function (a) {
	return {ctor: 'OutOfRange', _0: a};
};
var _fredcy$elm_parseint$ParseInt$InvalidChar = function (a) {
	return {ctor: 'InvalidChar', _0: a};
};
var _fredcy$elm_parseint$ParseInt$intFromChar = F2(
	function (radix, c) {
		var validInt = function (i) {
			return (_elm_lang$core$Native_Utils.cmp(i, radix) < 0) ? _elm_lang$core$Result$Ok(i) : _elm_lang$core$Result$Err(
				_fredcy$elm_parseint$ParseInt$OutOfRange(c));
		};
		var toInt = A3(
			_fredcy$elm_parseint$ParseInt$isBetween,
			_elm_lang$core$Native_Utils.chr('0'),
			_elm_lang$core$Native_Utils.chr('9'),
			c) ? _elm_lang$core$Result$Ok(
			A2(
				_fredcy$elm_parseint$ParseInt$charOffset,
				_elm_lang$core$Native_Utils.chr('0'),
				c)) : (A3(
			_fredcy$elm_parseint$ParseInt$isBetween,
			_elm_lang$core$Native_Utils.chr('a'),
			_elm_lang$core$Native_Utils.chr('z'),
			c) ? _elm_lang$core$Result$Ok(
			10 + A2(
				_fredcy$elm_parseint$ParseInt$charOffset,
				_elm_lang$core$Native_Utils.chr('a'),
				c)) : (A3(
			_fredcy$elm_parseint$ParseInt$isBetween,
			_elm_lang$core$Native_Utils.chr('A'),
			_elm_lang$core$Native_Utils.chr('Z'),
			c) ? _elm_lang$core$Result$Ok(
			10 + A2(
				_fredcy$elm_parseint$ParseInt$charOffset,
				_elm_lang$core$Native_Utils.chr('A'),
				c)) : _elm_lang$core$Result$Err(
			_fredcy$elm_parseint$ParseInt$InvalidChar(c))));
		return A2(_elm_lang$core$Result$andThen, validInt, toInt);
	});
var _fredcy$elm_parseint$ParseInt$parseIntR = F2(
	function (radix, rstring) {
		var _p0 = _elm_lang$core$String$uncons(rstring);
		if (_p0.ctor === 'Nothing') {
			return _elm_lang$core$Result$Ok(0);
		} else {
			return A2(
				_elm_lang$core$Result$andThen,
				function (ci) {
					return A2(
						_elm_lang$core$Result$andThen,
						function (ri) {
							return _elm_lang$core$Result$Ok(ci + (ri * radix));
						},
						A2(_fredcy$elm_parseint$ParseInt$parseIntR, radix, _p0._0._1));
				},
				A2(_fredcy$elm_parseint$ParseInt$intFromChar, radix, _p0._0._0));
		}
	});
var _fredcy$elm_parseint$ParseInt$parseIntRadix = F2(
	function (radix, string) {
		return ((_elm_lang$core$Native_Utils.cmp(2, radix) < 1) && (_elm_lang$core$Native_Utils.cmp(radix, 36) < 1)) ? A2(
			_fredcy$elm_parseint$ParseInt$parseIntR,
			radix,
			_elm_lang$core$String$reverse(string)) : _elm_lang$core$Result$Err(
			_fredcy$elm_parseint$ParseInt$InvalidRadix(radix));
	});
var _fredcy$elm_parseint$ParseInt$parseInt = _fredcy$elm_parseint$ParseInt$parseIntRadix(10);
var _fredcy$elm_parseint$ParseInt$parseIntOct = _fredcy$elm_parseint$ParseInt$parseIntRadix(8);
var _fredcy$elm_parseint$ParseInt$parseIntHex = _fredcy$elm_parseint$ParseInt$parseIntRadix(16);

var _eskimoblood$elm_color_extra$Color_Convert$xyzToColor = function (_p0) {
	var _p1 = _p0;
	var c = function (ch) {
		var ch_ = (_elm_lang$core$Native_Utils.cmp(ch, 3.1308e-3) > 0) ? ((1.055 * Math.pow(ch, 1 / 2.4)) - 5.5e-2) : (12.92 * ch);
		return _elm_lang$core$Basics$round(
			A3(_elm_lang$core$Basics$clamp, 0, 255, ch_ * 255));
	};
	var z_ = _p1.z / 100;
	var y_ = _p1.y / 100;
	var x_ = _p1.x / 100;
	var r = ((x_ * 3.2404542) + (y_ * -1.5371385)) + (z_ * -0.4986);
	var g = ((x_ * -0.969266) + (y_ * 1.8760108)) + (z_ * 4.1556e-2);
	var b = ((x_ * 5.56434e-2) + (y_ * -0.2040259)) + (z_ * 1.0572252);
	return A3(
		_elm_lang$core$Color$rgb,
		c(r),
		c(g),
		c(b));
};
var _eskimoblood$elm_color_extra$Color_Convert$labToXyz = function (_p2) {
	var _p3 = _p2;
	var y = (_p3.l + 16) / 116;
	var c = function (ch) {
		var ch_ = (ch * ch) * ch;
		return (_elm_lang$core$Native_Utils.cmp(ch_, 8.856e-3) > 0) ? ch_ : ((ch - (16 / 116)) / 7.787);
	};
	return {
		y: c(y) * 100,
		x: c(y + (_p3.a / 500)) * 95.047,
		z: c(y - (_p3.b / 200)) * 108.883
	};
};
var _eskimoblood$elm_color_extra$Color_Convert$labToColor = function (_p4) {
	return _eskimoblood$elm_color_extra$Color_Convert$xyzToColor(
		_eskimoblood$elm_color_extra$Color_Convert$labToXyz(_p4));
};
var _eskimoblood$elm_color_extra$Color_Convert$xyzToLab = function (_p5) {
	var _p6 = _p5;
	var c = function (ch) {
		return (_elm_lang$core$Native_Utils.cmp(ch, 8.856e-3) > 0) ? Math.pow(ch, 1 / 3) : ((7.787 * ch) + (16 / 116));
	};
	var x_ = c(_p6.x / 95.047);
	var y_ = c(_p6.y / 100);
	var z_ = c(_p6.z / 108.883);
	return {l: (116 * y_) - 16, a: 500 * (x_ - y_), b: 200 * (y_ - z_)};
};
var _eskimoblood$elm_color_extra$Color_Convert$colorToXyz = function (cl) {
	var _p7 = _elm_lang$core$Color$toRgb(cl);
	var red = _p7.red;
	var green = _p7.green;
	var blue = _p7.blue;
	var c = function (ch) {
		var ch_ = _elm_lang$core$Basics$toFloat(ch) / 255;
		var ch__ = (_elm_lang$core$Native_Utils.cmp(ch_, 4.045e-2) > 0) ? Math.pow((ch_ + 5.5e-2) / 1.055, 2.4) : (ch_ / 12.92);
		return ch__ * 100;
	};
	var r = c(red);
	var g = c(green);
	var b = c(blue);
	return {x: ((r * 0.4124) + (g * 0.3576)) + (b * 0.1805), y: ((r * 0.2126) + (g * 0.7152)) + (b * 7.22e-2), z: ((r * 1.93e-2) + (g * 0.1192)) + (b * 0.9505)};
};
var _eskimoblood$elm_color_extra$Color_Convert$colorToLab = function (_p8) {
	return _eskimoblood$elm_color_extra$Color_Convert$xyzToLab(
		_eskimoblood$elm_color_extra$Color_Convert$colorToXyz(_p8));
};
var _eskimoblood$elm_color_extra$Color_Convert$toRadix = function (n) {
	var getChr = function (c) {
		return (_elm_lang$core$Native_Utils.cmp(c, 10) < 0) ? _elm_lang$core$Basics$toString(c) : _elm_lang$core$String$fromChar(
			_elm_lang$core$Char$fromCode(87 + c));
	};
	return (_elm_lang$core$Native_Utils.cmp(n, 16) < 0) ? getChr(n) : A2(
		_elm_lang$core$Basics_ops['++'],
		_eskimoblood$elm_color_extra$Color_Convert$toRadix((n / 16) | 0),
		getChr(
			A2(_elm_lang$core$Basics_ops['%'], n, 16)));
};
var _eskimoblood$elm_color_extra$Color_Convert$toHex = function (_p9) {
	return A3(
		_elm_lang$core$String$padLeft,
		2,
		_elm_lang$core$Native_Utils.chr('0'),
		_eskimoblood$elm_color_extra$Color_Convert$toRadix(_p9));
};
var _eskimoblood$elm_color_extra$Color_Convert$colorToHex = function (cl) {
	var _p10 = _elm_lang$core$Color$toRgb(cl);
	var red = _p10.red;
	var green = _p10.green;
	var blue = _p10.blue;
	return A2(
		_elm_lang$core$String$join,
		'',
		A2(
			F2(
				function (x, y) {
					return {ctor: '::', _0: x, _1: y};
				}),
			'#',
			A2(
				_elm_lang$core$List$map,
				_eskimoblood$elm_color_extra$Color_Convert$toHex,
				{
					ctor: '::',
					_0: red,
					_1: {
						ctor: '::',
						_0: green,
						_1: {
							ctor: '::',
							_0: blue,
							_1: {ctor: '[]'}
						}
					}
				})));
};
var _eskimoblood$elm_color_extra$Color_Convert$hexToColor = function () {
	var pattern = A2(
		_elm_lang$core$Basics_ops['++'],
		'',
		A2(
			_elm_lang$core$Basics_ops['++'],
			'^',
			A2(
				_elm_lang$core$Basics_ops['++'],
				'#?',
				A2(
					_elm_lang$core$Basics_ops['++'],
					'(?:',
					A2(
						_elm_lang$core$Basics_ops['++'],
						'(?:([a-f\\d]{2})([a-f\\d]{2})([a-f\\d]{2}))',
						A2(
							_elm_lang$core$Basics_ops['++'],
							'|',
							A2(
								_elm_lang$core$Basics_ops['++'],
								'(?:([a-f\\d])([a-f\\d])([a-f\\d]))',
								A2(_elm_lang$core$Basics_ops['++'], ')', '$'))))))));
	var extend = function (token) {
		var _p11 = _elm_lang$core$String$toList(token);
		if ((_p11.ctor === '::') && (_p11._1.ctor === '[]')) {
			var _p12 = _p11._0;
			return _elm_lang$core$String$fromList(
				{
					ctor: '::',
					_0: _p12,
					_1: {
						ctor: '::',
						_0: _p12,
						_1: {ctor: '[]'}
					}
				});
		} else {
			return token;
		}
	};
	return function (_p13) {
		return A2(
			_elm_lang$core$Result$andThen,
			function (colors) {
				var _p15 = A2(
					_elm_lang$core$List$map,
					function (_p14) {
						return _fredcy$elm_parseint$ParseInt$parseIntHex(
							extend(_p14));
					},
					colors);
				if (((((((_p15.ctor === '::') && (_p15._0.ctor === 'Ok')) && (_p15._1.ctor === '::')) && (_p15._1._0.ctor === 'Ok')) && (_p15._1._1.ctor === '::')) && (_p15._1._1._0.ctor === 'Ok')) && (_p15._1._1._1.ctor === '[]')) {
					return _elm_lang$core$Result$Ok(
						A3(_elm_lang$core$Color$rgb, _p15._0._0, _p15._1._0._0, _p15._1._1._0._0));
				} else {
					return _elm_lang$core$Result$Err('Parsing ints from hex failed');
				}
			},
			A2(
				_elm_lang$core$Result$fromMaybe,
				'Parsing hex regex failed',
				A2(
					_elm_lang$core$Maybe$map,
					_elm_lang$core$List$filterMap(_elm_lang$core$Basics$identity),
					A2(
						_elm_lang$core$Maybe$map,
						function (_) {
							return _.submatches;
						},
						_elm_lang$core$List$head(
							A3(
								_elm_lang$core$Regex$find,
								_elm_lang$core$Regex$AtMost(1),
								_elm_lang$core$Regex$regex(pattern),
								_elm_lang$core$String$toLower(_p13)))))));
	};
}();
var _eskimoblood$elm_color_extra$Color_Convert$cssColorString = F2(
	function (kind, values) {
		return A2(
			_elm_lang$core$Basics_ops['++'],
			kind,
			A2(
				_elm_lang$core$Basics_ops['++'],
				'(',
				A2(
					_elm_lang$core$Basics_ops['++'],
					A2(_elm_lang$core$String$join, ', ', values),
					')')));
	});
var _eskimoblood$elm_color_extra$Color_Convert$toPercentString = function (_p16) {
	return A3(
		_elm_lang$core$Basics$flip,
		F2(
			function (x, y) {
				return A2(_elm_lang$core$Basics_ops['++'], x, y);
			}),
		'%',
		_elm_lang$core$Basics$toString(
			_elm_lang$core$Basics$round(
				A2(
					F2(
						function (x, y) {
							return x * y;
						}),
					100,
					_p16))));
};
var _eskimoblood$elm_color_extra$Color_Convert$hueToString = function (_p17) {
	return _elm_lang$core$Basics$toString(
		_elm_lang$core$Basics$round(
			A3(
				_elm_lang$core$Basics$flip,
				F2(
					function (x, y) {
						return x / y;
					}),
				_elm_lang$core$Basics$pi,
				A2(
					F2(
						function (x, y) {
							return x * y;
						}),
					180,
					_p17))));
};
var _eskimoblood$elm_color_extra$Color_Convert$colorToCssHsla = function (cl) {
	var _p18 = _elm_lang$core$Color$toHsl(cl);
	var hue = _p18.hue;
	var saturation = _p18.saturation;
	var lightness = _p18.lightness;
	var alpha = _p18.alpha;
	return A2(
		_eskimoblood$elm_color_extra$Color_Convert$cssColorString,
		'hsla',
		{
			ctor: '::',
			_0: _eskimoblood$elm_color_extra$Color_Convert$hueToString(hue),
			_1: {
				ctor: '::',
				_0: _eskimoblood$elm_color_extra$Color_Convert$toPercentString(saturation),
				_1: {
					ctor: '::',
					_0: _eskimoblood$elm_color_extra$Color_Convert$toPercentString(lightness),
					_1: {
						ctor: '::',
						_0: _elm_lang$core$Basics$toString(alpha),
						_1: {ctor: '[]'}
					}
				}
			}
		});
};
var _eskimoblood$elm_color_extra$Color_Convert$colorToCssHsl = function (cl) {
	var _p19 = _elm_lang$core$Color$toHsl(cl);
	var hue = _p19.hue;
	var saturation = _p19.saturation;
	var lightness = _p19.lightness;
	var alpha = _p19.alpha;
	return A2(
		_eskimoblood$elm_color_extra$Color_Convert$cssColorString,
		'hsl',
		{
			ctor: '::',
			_0: _eskimoblood$elm_color_extra$Color_Convert$hueToString(hue),
			_1: {
				ctor: '::',
				_0: _eskimoblood$elm_color_extra$Color_Convert$toPercentString(saturation),
				_1: {
					ctor: '::',
					_0: _eskimoblood$elm_color_extra$Color_Convert$toPercentString(lightness),
					_1: {ctor: '[]'}
				}
			}
		});
};
var _eskimoblood$elm_color_extra$Color_Convert$colorToCssRgba = function (cl) {
	var _p20 = _elm_lang$core$Color$toRgb(cl);
	var red = _p20.red;
	var green = _p20.green;
	var blue = _p20.blue;
	var alpha = _p20.alpha;
	return A2(
		_eskimoblood$elm_color_extra$Color_Convert$cssColorString,
		'rgba',
		{
			ctor: '::',
			_0: _elm_lang$core$Basics$toString(red),
			_1: {
				ctor: '::',
				_0: _elm_lang$core$Basics$toString(green),
				_1: {
					ctor: '::',
					_0: _elm_lang$core$Basics$toString(blue),
					_1: {
						ctor: '::',
						_0: _elm_lang$core$Basics$toString(alpha),
						_1: {ctor: '[]'}
					}
				}
			}
		});
};
var _eskimoblood$elm_color_extra$Color_Convert$colorToCssRgb = function (cl) {
	var _p21 = _elm_lang$core$Color$toRgb(cl);
	var red = _p21.red;
	var green = _p21.green;
	var blue = _p21.blue;
	var alpha = _p21.alpha;
	return A2(
		_eskimoblood$elm_color_extra$Color_Convert$cssColorString,
		'rgb',
		{
			ctor: '::',
			_0: _elm_lang$core$Basics$toString(red),
			_1: {
				ctor: '::',
				_0: _elm_lang$core$Basics$toString(green),
				_1: {
					ctor: '::',
					_0: _elm_lang$core$Basics$toString(blue),
					_1: {ctor: '[]'}
				}
			}
		});
};
var _eskimoblood$elm_color_extra$Color_Convert$XYZ = F3(
	function (a, b, c) {
		return {x: a, y: b, z: c};
	});
var _eskimoblood$elm_color_extra$Color_Convert$Lab = F3(
	function (a, b, c) {
		return {l: a, a: b, b: c};
	});

var _eskimoblood$elm_color_extra$Color_Interpolate$linear = F3(
	function (t, i1, i2) {
		return i1 + ((i2 - i1) * t);
	});
var _eskimoblood$elm_color_extra$Color_Interpolate$degree360 = _elm_lang$core$Basics$degrees(360);
var _eskimoblood$elm_color_extra$Color_Interpolate$degree180 = _elm_lang$core$Basics$degrees(180);
var _eskimoblood$elm_color_extra$Color_Interpolate$interpolate = F4(
	function (space, cl1, cl2, t) {
		var i = _eskimoblood$elm_color_extra$Color_Interpolate$linear(t);
		var _p0 = space;
		switch (_p0.ctor) {
			case 'RGB':
				var cl2_ = _elm_lang$core$Color$toRgb(cl2);
				var cl1_ = _elm_lang$core$Color$toRgb(cl1);
				return A4(
					_elm_lang$core$Color$rgba,
					_elm_lang$core$Basics$round(
						A2(
							i,
							_elm_lang$core$Basics$toFloat(cl1_.red),
							_elm_lang$core$Basics$toFloat(cl2_.red))),
					_elm_lang$core$Basics$round(
						A2(
							i,
							_elm_lang$core$Basics$toFloat(cl1_.green),
							_elm_lang$core$Basics$toFloat(cl2_.green))),
					_elm_lang$core$Basics$round(
						A2(
							i,
							_elm_lang$core$Basics$toFloat(cl1_.blue),
							_elm_lang$core$Basics$toFloat(cl2_.blue))),
					A2(i, cl1_.alpha, cl2_.alpha));
			case 'HSL':
				var cl2_ = _elm_lang$core$Color$toHsl(cl2);
				var h2 = cl2_.hue;
				var cl1_ = _elm_lang$core$Color$toHsl(cl1);
				var h1 = cl1_.hue;
				var dH = ((_elm_lang$core$Native_Utils.cmp(h2, h1) > 0) && (_elm_lang$core$Native_Utils.cmp(h2 - h1, _eskimoblood$elm_color_extra$Color_Interpolate$degree180) > 0)) ? ((h2 - h1) + _eskimoblood$elm_color_extra$Color_Interpolate$degree360) : (((_elm_lang$core$Native_Utils.cmp(h2, h1) < 0) && (_elm_lang$core$Native_Utils.cmp(h1 - h2, _eskimoblood$elm_color_extra$Color_Interpolate$degree180) > 0)) ? ((h2 + _eskimoblood$elm_color_extra$Color_Interpolate$degree360) - h1) : (h2 - h1));
				return A4(
					_elm_lang$core$Color$hsla,
					h1 + (t * dH),
					A2(i, cl1_.saturation, cl2_.saturation),
					A2(i, cl1_.lightness, cl2_.lightness),
					A2(i, cl1_.alpha, cl2_.alpha));
			default:
				var lab2 = _eskimoblood$elm_color_extra$Color_Convert$colorToLab(cl2);
				var lab1 = _eskimoblood$elm_color_extra$Color_Convert$colorToLab(cl1);
				return _eskimoblood$elm_color_extra$Color_Convert$labToColor(
					{
						l: A2(i, lab1.l, lab2.l),
						a: A2(i, lab1.a, lab2.a),
						b: A2(i, lab1.b, lab2.b)
					});
		}
	});
var _eskimoblood$elm_color_extra$Color_Interpolate$LAB = {ctor: 'LAB'};
var _eskimoblood$elm_color_extra$Color_Interpolate$HSL = {ctor: 'HSL'};
var _eskimoblood$elm_color_extra$Color_Interpolate$RGB = {ctor: 'RGB'};

var _eskimoblood$elm_color_extra$Color_Gradient$calcCosine = F5(
	function (a, b, c, d, t) {
		return _elm_lang$core$Basics$round(
			A2(
				F2(
					function (x, y) {
						return x * y;
					}),
				255,
				A3(
					_elm_lang$core$Basics$clamp,
					0,
					1,
					a + (b * _elm_lang$core$Basics$cos((_elm_lang$core$Basics$pi * 2) * ((c * t) + d))))));
	});
var _eskimoblood$elm_color_extra$Color_Gradient$calcCosineColor = F5(
	function (_p3, _p2, _p1, _p0, t) {
		var _p4 = _p3;
		var _p5 = _p2;
		var _p6 = _p1;
		var _p7 = _p0;
		return A3(
			_elm_lang$core$Color$rgb,
			A5(_eskimoblood$elm_color_extra$Color_Gradient$calcCosine, _p4._0, _p5._0, _p6._0, _p7._0, t),
			A5(_eskimoblood$elm_color_extra$Color_Gradient$calcCosine, _p4._1, _p5._1, _p6._1, _p7._1, t),
			A5(_eskimoblood$elm_color_extra$Color_Gradient$calcCosine, _p4._2, _p5._2, _p6._2, _p7._2, t));
	});
var _eskimoblood$elm_color_extra$Color_Gradient$cosineGradient = F5(
	function (offset, amp, fmod, phase, l) {
		return A2(
			_elm_lang$core$List$map,
			A4(_eskimoblood$elm_color_extra$Color_Gradient$calcCosineColor, offset, amp, fmod, phase),
			A2(
				_elm_lang$core$List$map,
				function (_p8) {
					return A2(
						F2(
							function (x, y) {
								return x * y;
							}),
						1.0 / _elm_lang$core$Basics$toFloat(l),
						_elm_lang$core$Basics$toFloat(_p8));
				},
				A2(_elm_lang$core$List$range, 0, l)));
	});
var _eskimoblood$elm_color_extra$Color_Gradient$getNextGradientStop = F2(
	function (currentStop, gradient) {
		var nextStop = _elm_lang$core$List$head(gradient);
		var _p9 = nextStop;
		if (_p9.ctor === 'Just') {
			return {
				ctor: '_Tuple2',
				_0: _p9._0,
				_1: A2(
					_elm_lang$core$Maybe$withDefault,
					{ctor: '[]'},
					_elm_lang$core$List$tail(gradient))
			};
		} else {
			return {ctor: '_Tuple2', _0: currentStop, _1: gradient};
		}
	});
var _eskimoblood$elm_color_extra$Color_Gradient$calculateColor = F4(
	function (space, _p11, _p10, t) {
		var _p12 = _p11;
		var _p16 = _p12._0;
		var _p15 = _p12._1;
		var _p13 = _p10;
		var _p14 = _p13._1;
		return _elm_lang$core$Native_Utils.eq(t, 0) ? _p15 : (_elm_lang$core$Native_Utils.eq(t, 1) ? _p14 : A4(_eskimoblood$elm_color_extra$Color_Interpolate$interpolate, space, _p15, _p14, (t - _p16) / (_p13._0 - _p16)));
	});
var _eskimoblood$elm_color_extra$Color_Gradient$calculateGradient = F5(
	function (space, stop1, stop2, gradient, t) {
		if (_elm_lang$core$Native_Utils.cmp(
			_elm_lang$core$Tuple$first(stop2),
			t) < 0) {
			var _p17 = A2(_eskimoblood$elm_color_extra$Color_Gradient$getNextGradientStop, stop2, gradient);
			var stop2_ = _p17._0;
			var gradient_ = _p17._1;
			var stop1_ = stop2;
			return {
				ctor: '_Tuple4',
				_0: stop1_,
				_1: stop2_,
				_2: gradient_,
				_3: A4(_eskimoblood$elm_color_extra$Color_Gradient$calculateColor, space, stop1_, stop2_, t)
			};
		} else {
			return {
				ctor: '_Tuple4',
				_0: stop1,
				_1: stop2,
				_2: gradient,
				_3: A4(_eskimoblood$elm_color_extra$Color_Gradient$calculateColor, space, stop1, stop2, t)
			};
		}
	});
var _eskimoblood$elm_color_extra$Color_Gradient$c = F3(
	function (space, t, _p18) {
		var _p19 = _p18;
		var _p20 = A5(_eskimoblood$elm_color_extra$Color_Gradient$calculateGradient, space, _p19._0, _p19._1, _p19._2, t);
		var stop1_ = _p20._0;
		var stop2_ = _p20._1;
		var gradient_ = _p20._2;
		var color = _p20._3;
		return {
			ctor: '_Tuple4',
			_0: stop1_,
			_1: stop2_,
			_2: gradient_,
			_3: {ctor: '::', _0: color, _1: _p19._3}
		};
	});
var _eskimoblood$elm_color_extra$Color_Gradient$linearGradientFromStops = F3(
	function (space, stops, size) {
		var purifiedStops = A2(
			_elm_lang$core$List$sortBy,
			function (_p21) {
				var _p22 = _p21;
				return _p22._0;
			},
			A2(
				_elm_lang$core$List$filter,
				function (_p23) {
					var _p24 = _p23;
					var _p25 = _p24._0;
					return (_elm_lang$core$Native_Utils.cmp(_p25, 0) > -1) && (_elm_lang$core$Native_Utils.cmp(_p25, 1) < 1);
				},
				stops));
		var stop1 = _elm_lang$core$List$head(purifiedStops);
		var _p26 = stop1;
		if (_p26.ctor === 'Just') {
			var _p30 = _p26._0;
			var currentStops = A2(
				_elm_lang$core$Maybe$withDefault,
				{ctor: '[]'},
				_elm_lang$core$List$tail(purifiedStops));
			var _p27 = A2(_eskimoblood$elm_color_extra$Color_Gradient$getNextGradientStop, _p30, currentStops);
			var s2 = _p27._0;
			var g = _p27._1;
			var l = size - 1;
			var stops = A2(
				_elm_lang$core$List$map,
				function (i) {
					return _elm_lang$core$Basics$toFloat(i) / _elm_lang$core$Basics$toFloat(l);
				},
				A2(_elm_lang$core$List$range, 0, l));
			return _elm_lang$core$List$reverse(
				function (_p28) {
					var _p29 = _p28;
					return _p29._3;
				}(
					A3(
						_elm_lang$core$List$foldl,
						_eskimoblood$elm_color_extra$Color_Gradient$c(space),
						{
							ctor: '_Tuple4',
							_0: _p30,
							_1: s2,
							_2: g,
							_3: {ctor: '[]'}
						},
						stops)));
		} else {
			return {ctor: '[]'};
		}
	});
var _eskimoblood$elm_color_extra$Color_Gradient$linearGradient = F3(
	function (space, palette, size) {
		var l = _elm_lang$core$List$length(palette) - 1;
		var gr = A3(
			_elm_lang$core$List$map2,
			F2(
				function (i, cl) {
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Basics$toFloat(i) / _elm_lang$core$Basics$toFloat(l),
						_1: cl
					};
				}),
			A2(_elm_lang$core$List$range, 0, l),
			palette);
		return A3(_eskimoblood$elm_color_extra$Color_Gradient$linearGradientFromStops, space, gr, size);
	});

var _klaftertief$elm_heatmap$Heatmap_Gradient$visibleSpectrum = {
	ctor: '::',
	_0: {
		ctor: '_Tuple2',
		_0: 0,
		_1: A3(_elm_lang$core$Color$rgb, 255, 0, 255)
	},
	_1: {
		ctor: '::',
		_0: {
			ctor: '_Tuple2',
			_0: 0.25,
			_1: A3(_elm_lang$core$Color$rgb, 0, 0, 255)
		},
		_1: {
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: 0.5,
				_1: A3(_elm_lang$core$Color$rgb, 0, 255, 0)
			},
			_1: {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 0.75,
					_1: A3(_elm_lang$core$Color$rgb, 255, 255, 0)
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 1,
						_1: A3(_elm_lang$core$Color$rgb, 255, 0, 0)
					},
					_1: {ctor: '[]'}
				}
			}
		}
	}
};
var _klaftertief$elm_heatmap$Heatmap_Gradient$sunrise = {
	ctor: '::',
	_0: {
		ctor: '_Tuple2',
		_0: 0,
		_1: A3(_elm_lang$core$Color$rgb, 255, 0, 0)
	},
	_1: {
		ctor: '::',
		_0: {
			ctor: '_Tuple2',
			_0: 0.66,
			_1: A3(_elm_lang$core$Color$rgb, 255, 255, 0)
		},
		_1: {
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: 1,
				_1: A3(_elm_lang$core$Color$rgb, 255, 255, 255)
			},
			_1: {ctor: '[]'}
		}
	}
};
var _klaftertief$elm_heatmap$Heatmap_Gradient$steppedColors = {
	ctor: '::',
	_0: {
		ctor: '_Tuple2',
		_0: 0,
		_1: A3(_elm_lang$core$Color$rgb, 0, 0, 128)
	},
	_1: {
		ctor: '::',
		_0: {
			ctor: '_Tuple2',
			_0: 0.25,
			_1: A3(_elm_lang$core$Color$rgb, 0, 0, 128)
		},
		_1: {
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: 0.26,
				_1: A3(_elm_lang$core$Color$rgb, 0, 128, 0)
			},
			_1: {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 0.5,
					_1: A3(_elm_lang$core$Color$rgb, 0, 128, 0)
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 0.51,
						_1: A3(_elm_lang$core$Color$rgb, 255, 255, 0)
					},
					_1: {
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 0.75,
							_1: A3(_elm_lang$core$Color$rgb, 255, 255, 0)
						},
						_1: {
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: 0.76,
								_1: A3(_elm_lang$core$Color$rgb, 255, 0, 0)
							},
							_1: {
								ctor: '::',
								_0: {
									ctor: '_Tuple2',
									_0: 1,
									_1: A3(_elm_lang$core$Color$rgb, 255, 0, 0)
								},
								_1: {ctor: '[]'}
							}
						}
					}
				}
			}
		}
	}
};
var _klaftertief$elm_heatmap$Heatmap_Gradient$heatedMetal = {
	ctor: '::',
	_0: {
		ctor: '_Tuple2',
		_0: 0,
		_1: A3(_elm_lang$core$Color$rgb, 0, 0, 0)
	},
	_1: {
		ctor: '::',
		_0: {
			ctor: '_Tuple2',
			_0: 0.4,
			_1: A3(_elm_lang$core$Color$rgb, 128, 0, 128)
		},
		_1: {
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: 0.6,
				_1: A3(_elm_lang$core$Color$rgb, 255, 0, 0)
			},
			_1: {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 0.8,
					_1: A3(_elm_lang$core$Color$rgb, 255, 255, 0)
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 1,
						_1: A3(_elm_lang$core$Color$rgb, 255, 255, 255)
					},
					_1: {ctor: '[]'}
				}
			}
		}
	}
};
var _klaftertief$elm_heatmap$Heatmap_Gradient$incandescent = {
	ctor: '::',
	_0: {
		ctor: '_Tuple2',
		_0: 0,
		_1: A3(_elm_lang$core$Color$rgb, 0, 0, 0)
	},
	_1: {
		ctor: '::',
		_0: {
			ctor: '_Tuple2',
			_0: 0.33,
			_1: A3(_elm_lang$core$Color$rgb, 0, 0, 255)
		},
		_1: {
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: 0.66,
				_1: A3(_elm_lang$core$Color$rgb, 139, 0, 0)
			},
			_1: {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 1,
					_1: A3(_elm_lang$core$Color$rgb, 255, 255, 255)
				},
				_1: {ctor: '[]'}
			}
		}
	}
};
var _klaftertief$elm_heatmap$Heatmap_Gradient$deepSea = {
	ctor: '::',
	_0: {
		ctor: '_Tuple2',
		_0: 0,
		_1: A3(_elm_lang$core$Color$rgb, 0, 0, 0)
	},
	_1: {
		ctor: '::',
		_0: {
			ctor: '_Tuple2',
			_0: 0.6,
			_1: A3(_elm_lang$core$Color$rgb, 24, 53, 103)
		},
		_1: {
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: 0.75,
				_1: A3(_elm_lang$core$Color$rgb, 46, 100, 158)
			},
			_1: {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 0.9,
					_1: A3(_elm_lang$core$Color$rgb, 23, 173, 201)
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 1,
						_1: A3(_elm_lang$core$Color$rgb, 0, 250, 250)
					},
					_1: {ctor: '[]'}
				}
			}
		}
	}
};
var _klaftertief$elm_heatmap$Heatmap_Gradient$colorSpectrum = {
	ctor: '::',
	_0: {
		ctor: '_Tuple2',
		_0: 0,
		_1: A3(_elm_lang$core$Color$rgb, 0, 0, 128)
	},
	_1: {
		ctor: '::',
		_0: {
			ctor: '_Tuple2',
			_0: 0.25,
			_1: A3(_elm_lang$core$Color$rgb, 0, 0, 255)
		},
		_1: {
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: 0.5,
				_1: A3(_elm_lang$core$Color$rgb, 0, 128, 0)
			},
			_1: {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 0.75,
					_1: A3(_elm_lang$core$Color$rgb, 255, 255, 0)
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 1,
						_1: A3(_elm_lang$core$Color$rgb, 255, 0, 0)
					},
					_1: {ctor: '[]'}
				}
			}
		}
	}
};
var _klaftertief$elm_heatmap$Heatmap_Gradient$blueRed = {
	ctor: '::',
	_0: {
		ctor: '_Tuple2',
		_0: 0,
		_1: A3(_elm_lang$core$Color$rgb, 0, 0, 255)
	},
	_1: {
		ctor: '::',
		_0: {
			ctor: '_Tuple2',
			_0: 1,
			_1: A3(_elm_lang$core$Color$rgb, 255, 0, 0)
		},
		_1: {ctor: '[]'}
	}
};
var _klaftertief$elm_heatmap$Heatmap_Gradient$blackAquaWhite = {
	ctor: '::',
	_0: {
		ctor: '_Tuple2',
		_0: 0,
		_1: A3(_elm_lang$core$Color$rgb, 0, 0, 0)
	},
	_1: {
		ctor: '::',
		_0: {
			ctor: '_Tuple2',
			_0: 0.6,
			_1: A3(_elm_lang$core$Color$rgb, 0, 255, 255)
		},
		_1: {
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: 1,
				_1: A3(_elm_lang$core$Color$rgb, 255, 255, 255)
			},
			_1: {ctor: '[]'}
		}
	}
};

var _klaftertief$elm_heatmap$Heatmap$mergePoints = F2(
	function (p1, p2) {
		return {x: ((p1.x * p1.weight) + (p2.x * p2.weight)) / (p1.weight + p2.weight), y: ((p1.y * p1.weight) + (p2.y * p2.weight)) / (p1.weight + p2.weight), weight: p1.weight + p2.weight};
	});
var _klaftertief$elm_heatmap$Heatmap$clusterHelp = F3(
	function (_p0, grid, points) {
		clusterHelp:
		while (true) {
			var _p1 = _p0;
			var _p2 = points;
			if (_p2.ctor === '::') {
				var _p4 = _p2._0;
				var addPoint = function (maybeExistingPoint) {
					var _p3 = maybeExistingPoint;
					if (_p3.ctor === 'Just') {
						return _elm_lang$core$Maybe$Just(
							A2(_klaftertief$elm_heatmap$Heatmap$mergePoints, _p3._0, _p4));
					} else {
						return _elm_lang$core$Maybe$Just(_p4);
					}
				};
				var cellSize = _p1._0.radius / 2;
				var xCell = _elm_lang$core$Basics$floor(_p2._0.x / cellSize);
				var yCell = _elm_lang$core$Basics$floor(_p2._0.y / cellSize);
				var newGrid = A3(
					_elm_lang$core$Dict$update,
					{ctor: '_Tuple2', _0: xCell, _1: yCell},
					addPoint,
					grid);
				var _v3 = _p1,
					_v4 = newGrid,
					_v5 = _p2._1;
				_p0 = _v3;
				grid = _v4;
				points = _v5;
				continue clusterHelp;
			} else {
				return grid;
			}
		}
	});
var _klaftertief$elm_heatmap$Heatmap$cluster = F2(
	function (config, points) {
		return _elm_lang$core$Dict$values(
			A3(_klaftertief$elm_heatmap$Heatmap$clusterHelp, config, _elm_lang$core$Dict$empty, points));
	});
var _klaftertief$elm_heatmap$Heatmap$identifier = F2(
	function (_p5, name) {
		var _p6 = _p5;
		var suffix = A2(
			_elm_lang$core$Maybe$withDefault,
			'',
			A2(
				_elm_lang$core$Maybe$map,
				F2(
					function (x, y) {
						return A2(_elm_lang$core$Basics_ops['++'], x, y);
					})('_'),
				_p6._0.idSuffix));
		return A2(
			_elm_lang$core$Basics_ops['++'],
			'heatmap',
			A2(_elm_lang$core$Basics_ops['++'], name, suffix));
	});
var _klaftertief$elm_heatmap$Heatmap$referenceIdentifier = F2(
	function (config, name) {
		return A2(
			_elm_lang$core$Basics_ops['++'],
			'#',
			A2(_klaftertief$elm_heatmap$Heatmap$identifier, config, name));
	});
var _klaftertief$elm_heatmap$Heatmap$linkToIdentifier = F2(
	function (config, name) {
		return A2(
			_elm_lang$core$Basics_ops['++'],
			'url(',
			A2(
				_elm_lang$core$Basics_ops['++'],
				A2(_klaftertief$elm_heatmap$Heatmap$referenceIdentifier, config, name),
				')'));
	});
var _klaftertief$elm_heatmap$Heatmap$pointGradientName = 'PointGradient';
var _klaftertief$elm_heatmap$Heatmap$pointName = 'Point';
var _klaftertief$elm_heatmap$Heatmap$filterName = 'Filter';
var _klaftertief$elm_heatmap$Heatmap$viewGradientComponentTransferFilter = function (gradient) {
	var componentTransferPalette = A2(
		_elm_lang$core$List$map,
		function (_p7) {
			var _p8 = _p7;
			return {
				red: A3(
					_elm_lang$core$Basics$clamp,
					0,
					1,
					_elm_lang$core$Basics$toFloat(_p8.red) / 256),
				green: A3(
					_elm_lang$core$Basics$clamp,
					0,
					1,
					_elm_lang$core$Basics$toFloat(_p8.green) / 256),
				blue: A3(
					_elm_lang$core$Basics$clamp,
					0,
					1,
					_elm_lang$core$Basics$toFloat(_p8.blue) / 256)
			};
		},
		A2(
			_elm_lang$core$List$map,
			_elm_lang$core$Color$toRgb,
			_elm_lang$core$List$reverse(
				A3(_eskimoblood$elm_color_extra$Color_Gradient$linearGradientFromStops, _eskimoblood$elm_color_extra$Color_Interpolate$RGB, gradient, 128))));
	var componentTransferTableValues = function (color) {
		return A2(
			_elm_lang$core$String$join,
			' ',
			A2(
				_elm_lang$core$List$map,
				function (_p9) {
					return _elm_lang$core$Basics$toString(
						color(_p9));
				},
				componentTransferPalette));
	};
	return A2(
		_elm_lang$svg$Svg$feComponentTransfer,
		{
			ctor: '::',
			_0: _elm_lang$svg$Svg_Attributes$in_('toTransfer'),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$svg$Svg$feFuncR,
				{
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$type_('table'),
					_1: {
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$tableValues(
							componentTransferTableValues(
								function (_) {
									return _.red;
								})),
						_1: {ctor: '[]'}
					}
				},
				{ctor: '[]'}),
			_1: {
				ctor: '::',
				_0: A2(
					_elm_lang$svg$Svg$feFuncG,
					{
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$type_('table'),
						_1: {
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$tableValues(
								componentTransferTableValues(
									function (_) {
										return _.green;
									})),
							_1: {ctor: '[]'}
						}
					},
					{ctor: '[]'}),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$svg$Svg$feFuncB,
						{
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$type_('table'),
							_1: {
								ctor: '::',
								_0: _elm_lang$svg$Svg_Attributes$tableValues(
									componentTransferTableValues(
										function (_) {
											return _.blue;
										})),
								_1: {ctor: '[]'}
							}
						},
						{ctor: '[]'}),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$svg$Svg$feFuncA,
							{
								ctor: '::',
								_0: _elm_lang$svg$Svg_Attributes$type_('table'),
								_1: {
									ctor: '::',
									_0: _elm_lang$svg$Svg_Attributes$tableValues('0 0 0.5 0.6 0.7 0.8 0.9 1'),
									_1: {ctor: '[]'}
								}
							},
							{ctor: '[]'}),
						_1: {ctor: '[]'}
					}
				}
			}
		});
};
var _klaftertief$elm_heatmap$Heatmap$viewFilter = function (_p10) {
	var _p11 = _p10;
	return A2(
		_elm_lang$svg$Svg$filter,
		{
			ctor: '::',
			_0: _elm_lang$svg$Svg_Attributes$id(
				A2(_klaftertief$elm_heatmap$Heatmap$identifier, _p11, _klaftertief$elm_heatmap$Heatmap$filterName)),
			_1: {
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$width('120%'),
				_1: {
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$height('120%'),
					_1: {
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$x('-10%'),
						_1: {
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$y('-10%'),
							_1: {
								ctor: '::',
								_0: _elm_lang$svg$Svg_Attributes$filterUnits('userSpaceOnUse'),
								_1: {ctor: '[]'}
							}
						}
					}
				}
			}
		},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$svg$Svg$feGaussianBlur,
				{
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$in_('SourceGraphic'),
					_1: {
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$stdDeviation(
							_elm_lang$core$Basics$toString(_p11._0.blur)),
						_1: {
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$result('blurred'),
							_1: {ctor: '[]'}
						}
					}
				},
				{ctor: '[]'}),
			_1: {
				ctor: '::',
				_0: A2(
					_elm_lang$svg$Svg$feFlood,
					{
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$floodColor('white'),
						_1: {
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$floodOpacity('0.1'),
							_1: {
								ctor: '::',
								_0: _elm_lang$svg$Svg_Attributes$result('flooded'),
								_1: {ctor: '[]'}
							}
						}
					},
					{ctor: '[]'}),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$svg$Svg$feBlend,
						{
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$in_('flooded'),
							_1: {
								ctor: '::',
								_0: _elm_lang$svg$Svg_Attributes$in2('blurred'),
								_1: {
									ctor: '::',
									_0: _elm_lang$svg$Svg_Attributes$mode('multiply'),
									_1: {
										ctor: '::',
										_0: _elm_lang$svg$Svg_Attributes$result('toTransfer'),
										_1: {ctor: '[]'}
									}
								}
							}
						},
						{ctor: '[]'}),
					_1: {
						ctor: '::',
						_0: _klaftertief$elm_heatmap$Heatmap$viewGradientComponentTransferFilter(_p11._0.gradient),
						_1: {ctor: '[]'}
					}
				}
			}
		});
};
var _klaftertief$elm_heatmap$Heatmap$viewDefinitions = function (_p12) {
	var _p13 = _p12;
	var _p14 = _p13;
	return A2(
		_elm_lang$svg$Svg$defs,
		{ctor: '[]'},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$svg$Svg$radialGradient,
				{
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$id(
						A2(_klaftertief$elm_heatmap$Heatmap$identifier, _p14, _klaftertief$elm_heatmap$Heatmap$pointGradientName)),
					_1: {ctor: '[]'}
				},
				{
					ctor: '::',
					_0: A2(
						_elm_lang$svg$Svg$stop,
						{
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$offset('0%'),
							_1: {
								ctor: '::',
								_0: _elm_lang$svg$Svg_Attributes$stopColor('black'),
								_1: {
									ctor: '::',
									_0: _elm_lang$svg$Svg_Attributes$stopOpacity('1'),
									_1: {ctor: '[]'}
								}
							}
						},
						{ctor: '[]'}),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$svg$Svg$stop,
							{
								ctor: '::',
								_0: _elm_lang$svg$Svg_Attributes$offset('100%'),
								_1: {
									ctor: '::',
									_0: _elm_lang$svg$Svg_Attributes$stopColor('black'),
									_1: {
										ctor: '::',
										_0: _elm_lang$svg$Svg_Attributes$stopOpacity('0'),
										_1: {ctor: '[]'}
									}
								}
							},
							{ctor: '[]'}),
						_1: {ctor: '[]'}
					}
				}),
			_1: {
				ctor: '::',
				_0: A2(
					_elm_lang$svg$Svg$circle,
					{
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$id(
							A2(_klaftertief$elm_heatmap$Heatmap$identifier, _p14, _klaftertief$elm_heatmap$Heatmap$pointName)),
						_1: {
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$r(
								_elm_lang$core$Basics$toString(_p13._0.radius)),
							_1: {
								ctor: '::',
								_0: _elm_lang$svg$Svg_Attributes$fill(
									A2(_klaftertief$elm_heatmap$Heatmap$linkToIdentifier, _p14, _klaftertief$elm_heatmap$Heatmap$pointGradientName)),
								_1: {ctor: '[]'}
							}
						}
					},
					{ctor: '[]'}),
				_1: {
					ctor: '::',
					_0: _klaftertief$elm_heatmap$Heatmap$viewFilter(_p14),
					_1: {ctor: '[]'}
				}
			}
		});
};
var _klaftertief$elm_heatmap$Heatmap$viewPoint = F2(
	function (_p16, _p15) {
		var _p17 = _p16;
		var _p18 = _p15;
		return A2(
			_elm_lang$svg$Svg$use,
			{
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$xlinkHref(
					A2(_klaftertief$elm_heatmap$Heatmap$referenceIdentifier, _p17, _klaftertief$elm_heatmap$Heatmap$pointName)),
				_1: {
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$x(
						_elm_lang$core$Basics$toString(_p18.x)),
					_1: {
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$y(
							_elm_lang$core$Basics$toString(_p18.y)),
						_1: {
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$fillOpacity(
								_elm_lang$core$Basics$toString(_p18.weight / _p17._0.maxWeight)),
							_1: {ctor: '[]'}
						}
					}
				}
			},
			{ctor: '[]'});
	});
var _klaftertief$elm_heatmap$Heatmap$viewData = F2(
	function (_p19, data) {
		var _p20 = _p19;
		var _p21 = _p20;
		return A2(
			_elm_lang$svg$Svg$g,
			{
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$filter(
					A2(_klaftertief$elm_heatmap$Heatmap$linkToIdentifier, _p21, _klaftertief$elm_heatmap$Heatmap$filterName)),
				_1: {ctor: '[]'}
			},
			A2(
				_elm_lang$core$List$map,
				_klaftertief$elm_heatmap$Heatmap$viewPoint(_p21),
				A2(
					_klaftertief$elm_heatmap$Heatmap$cluster,
					_p21,
					A2(_elm_lang$core$List$map, _p20._0.toPoint, data))));
	});
var _klaftertief$elm_heatmap$Heatmap$view = F2(
	function (config, data) {
		return A2(
			_elm_lang$svg$Svg$g,
			{ctor: '[]'},
			{
				ctor: '::',
				_0: _klaftertief$elm_heatmap$Heatmap$viewDefinitions(config),
				_1: {
					ctor: '::',
					_0: A2(_klaftertief$elm_heatmap$Heatmap$viewData, config, data),
					_1: {ctor: '[]'}
				}
			});
	});
var _klaftertief$elm_heatmap$Heatmap$ConfigInternal = F6(
	function (a, b, c, d, e, f) {
		return {toPoint: a, gradient: b, maxWeight: c, radius: d, blur: e, idSuffix: f};
	});
var _klaftertief$elm_heatmap$Heatmap$Point = F3(
	function (a, b, c) {
		return {x: a, y: b, weight: c};
	});
var _klaftertief$elm_heatmap$Heatmap$Config = function (a) {
	return {ctor: 'Config', _0: a};
};
var _klaftertief$elm_heatmap$Heatmap$config = function (_p22) {
	var _p23 = _p22;
	return _klaftertief$elm_heatmap$Heatmap$Config(
		{toPoint: _p23.toPoint, gradient: _p23.gradient, maxWeight: 1, radius: 25, blur: 15, idSuffix: _elm_lang$core$Maybe$Nothing});
};
var _klaftertief$elm_heatmap$Heatmap$withMaxWeight = F2(
	function (maxWeight, _p24) {
		var _p25 = _p24;
		return _klaftertief$elm_heatmap$Heatmap$Config(
			_elm_lang$core$Native_Utils.update(
				_p25._0,
				{maxWeight: maxWeight}));
	});
var _klaftertief$elm_heatmap$Heatmap$withRadius = F2(
	function (radius, _p26) {
		var _p27 = _p26;
		return _klaftertief$elm_heatmap$Heatmap$Config(
			_elm_lang$core$Native_Utils.update(
				_p27._0,
				{radius: radius}));
	});
var _klaftertief$elm_heatmap$Heatmap$withBlur = F2(
	function (blur, _p28) {
		var _p29 = _p28;
		return _klaftertief$elm_heatmap$Heatmap$Config(
			_elm_lang$core$Native_Utils.update(
				_p29._0,
				{blur: blur}));
	});
var _klaftertief$elm_heatmap$Heatmap$withIdSuffix = F2(
	function (idSuffix, _p30) {
		var _p31 = _p30;
		return _klaftertief$elm_heatmap$Heatmap$Config(
			_elm_lang$core$Native_Utils.update(
				_p31._0,
				{
					idSuffix: _elm_lang$core$Maybe$Just(idSuffix)
				}));
	});

var _klaftertief$elm_slippy_map$SlippyMap_Control_Attribution$control = F3(
	function (_p0, prefix, attributions) {
		var _p1 = _p0;
		var prefixText = A2(
			_elm_lang$core$Maybe$withDefault,
			'',
			A2(
				_elm_lang$core$Maybe$map,
				function (p) {
					return A2(_elm_lang$core$Basics_ops['++'], p, ' | ');
				},
				prefix));
		return A2(
			_elm_lang$svg$Svg$g,
			{
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$class('esm__atttribution'),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$svg$Svg$text_,
					{
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$x(
							_elm_lang$core$Basics$toString(_p1._0)),
						_1: {
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$y(
								_elm_lang$core$Basics$toString(_p1._1)),
							_1: {
								ctor: '::',
								_0: _elm_lang$svg$Svg_Attributes$dx('-4'),
								_1: {
									ctor: '::',
									_0: _elm_lang$svg$Svg_Attributes$dy('-4'),
									_1: {
										ctor: '::',
										_0: _elm_lang$svg$Svg_Attributes$textAnchor('end'),
										_1: {
											ctor: '::',
											_0: _elm_lang$svg$Svg_Attributes$fontFamily('sans-serif'),
											_1: {
												ctor: '::',
												_0: _elm_lang$svg$Svg_Attributes$fontSize('12px'),
												_1: {ctor: '[]'}
											}
										}
									}
								}
							}
						}
					},
					{
						ctor: '::',
						_0: _elm_lang$svg$Svg$text(
							A2(
								_elm_lang$core$Basics_ops['++'],
								prefixText,
								A2(_elm_lang$core$String$join, ', ', attributions))),
						_1: {ctor: '[]'}
					}),
				_1: {ctor: '[]'}
			});
	});

var _klaftertief$elm_slippy_map$SlippyMap_Geo_Coordinate$zoomTo = F2(
	function (zoom, coordinate) {
		var scale = Math.pow(2, zoom - coordinate.zoom);
		return {column: coordinate.column * scale, row: coordinate.row * scale, zoom: zoom};
	});
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Coordinate$center = function (_p0) {
	var _p1 = _p0;
	var _p2 = _p1._0;
	var endZoomed = A2(_klaftertief$elm_slippy_map$SlippyMap_Geo_Coordinate$zoomTo, _p2.zoom, _p1._1);
	return {column: (_p2.column + endZoomed.column) / 2, row: (_p2.row + endZoomed.row) / 2, zoom: _p2.zoom};
};
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Coordinate$Coordinate = F3(
	function (a, b, c) {
		return {column: a, row: b, zoom: c};
	});
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Coordinate$Bounds = F4(
	function (a, b, c, d) {
		return {topLeft: a, topRight: b, bottomRight: c, bottomLeft: d};
	});

var _klaftertief$elm_slippy_map$SlippyMap_Geo_Location$isInsideBounds = F2(
	function (_p1, _p0) {
		var _p2 = _p1;
		var _p7 = _p2.southWest;
		var _p6 = _p2.northEast;
		var _p3 = _p0;
		var _p5 = _p3.lon;
		var _p4 = _p3.lat;
		return (_elm_lang$core$Native_Utils.cmp(_p5, _p7.lon) > 0) && ((_elm_lang$core$Native_Utils.cmp(_p5, _p6.lon) < 0) && ((_elm_lang$core$Native_Utils.cmp(_p4, _p7.lat) > 0) && (_elm_lang$core$Native_Utils.cmp(_p4, _p6.lat) < 0)));
	});
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Location$wrap = function (_p8) {
	var _p9 = _p8;
	return _elm_lang$core$Native_Utils.update(
		_p9,
		{
			lon: A3(_elm_lang$core$Basics$clamp, -180, 180, _p9.lon)
		});
};
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Location$center = function (_p10) {
	var _p11 = _p10;
	var _p13 = _p11.southWest;
	var _p12 = _p11.northEast;
	return {lon: (_p13.lon + _p12.lon) / 2, lat: (_p13.lat + _p12.lat) / 2};
};
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Location$Location = F2(
	function (a, b) {
		return {lon: a, lat: b};
	});
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Location$Bounds = F2(
	function (a, b) {
		return {southWest: a, northEast: b};
	});

var _klaftertief$elm_slippy_map$SlippyMap_Geo_Point$divideBy = F2(
	function (k, p) {
		return {x: p.x / k, y: p.y / k};
	});
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Point$multiplyBy = F2(
	function (k, p) {
		return {x: p.x * k, y: p.y * k};
	});
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Point$subtract = F2(
	function (q, p) {
		return {x: p.x - q.x, y: p.y - q.y};
	});
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Point$distance = F2(
	function (q, p) {
		return _elm_lang$core$Basics$sqrt(
			function (_p0) {
				var _p1 = _p0;
				return Math.pow(_p1.x, 2) + Math.pow(_p1.y, 2);
			}(
				A2(_klaftertief$elm_slippy_map$SlippyMap_Geo_Point$subtract, q, p)));
	});
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Point$add = F2(
	function (q, p) {
		return {x: p.x + q.x, y: p.y + q.y};
	});
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Point$Point = F2(
	function (a, b) {
		return {x: a, y: b};
	});
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Point$Bounds = F2(
	function (a, b) {
		return {topLeft: a, bottomRight: b};
	});

var _klaftertief$elm_slippy_map$SlippyMap_Geo_Tile$uniqueHelp = F3(
	function (f, existing, remaining) {
		uniqueHelp:
		while (true) {
			var _p0 = remaining;
			if (_p0.ctor === '[]') {
				return {ctor: '[]'};
			} else {
				var _p2 = _p0._1;
				var _p1 = _p0._0;
				var computedFirst = f(_p1);
				if (A2(_elm_lang$core$Set$member, computedFirst, existing)) {
					var _v1 = f,
						_v2 = existing,
						_v3 = _p2;
					f = _v1;
					existing = _v2;
					remaining = _v3;
					continue uniqueHelp;
				} else {
					return {
						ctor: '::',
						_0: _p1,
						_1: A3(
							_klaftertief$elm_slippy_map$SlippyMap_Geo_Tile$uniqueHelp,
							f,
							A2(_elm_lang$core$Set$insert, computedFirst, existing),
							_p2)
					};
				}
			}
		}
	});
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Tile$uniqueBy = F2(
	function (f, list) {
		return A3(_klaftertief$elm_slippy_map$SlippyMap_Geo_Tile$uniqueHelp, f, _elm_lang$core$Set$empty, list);
	});
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Tile$normalize = function (_p3) {
	var _p4 = _p3;
	var _p5 = _p4.z;
	return {
		z: _p5,
		x: A2(
			_elm_lang$core$Basics_ops['%'],
			_p4.x,
			Math.pow(2, _p5)),
		y: A2(
			_elm_lang$core$Basics_ops['%'],
			_p4.y,
			Math.pow(2, _p5))
	};
};
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Tile$coordinateToTile = function (_p6) {
	var _p7 = _p6;
	return {
		z: _elm_lang$core$Basics$floor(_p7.zoom),
		x: _elm_lang$core$Basics$floor(_p7.column),
		y: _elm_lang$core$Basics$floor(_p7.row)
	};
};
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Tile$triangleCover = function (_p8) {
	var _p9 = _p8;
	var _p14 = _p9._2;
	var _p13 = _p9._1;
	var _p12 = _p9._0;
	var _p10 = {
		ctor: '_Tuple3',
		_0: _klaftertief$elm_slippy_map$SlippyMap_Geo_Coordinate$center(
			{ctor: '_Tuple2', _0: _p12, _1: _p13}),
		_1: _klaftertief$elm_slippy_map$SlippyMap_Geo_Coordinate$center(
			{ctor: '_Tuple2', _0: _p13, _1: _p14}),
		_2: _klaftertief$elm_slippy_map$SlippyMap_Geo_Coordinate$center(
			{ctor: '_Tuple2', _0: _p14, _1: _p12})
	};
	var c1c2Center = _p10._0;
	var c2c3Center = _p10._1;
	var c3c1Center = _p10._2;
	var _p11 = {
		ctor: '_Tuple3',
		_0: _klaftertief$elm_slippy_map$SlippyMap_Geo_Tile$coordinateToTile(_p12),
		_1: _klaftertief$elm_slippy_map$SlippyMap_Geo_Tile$coordinateToTile(_p13),
		_2: _klaftertief$elm_slippy_map$SlippyMap_Geo_Tile$coordinateToTile(_p14)
	};
	var c1Tile = _p11._0;
	var c2Tile = _p11._1;
	var c3Tile = _p11._2;
	return (A2(
		F2(
			function (x, y) {
				return _elm_lang$core$Native_Utils.cmp(x, y) > -1;
			}),
		1,
		_elm_lang$core$Basics$abs(c1Tile.x - c2Tile.x)) && (A2(
		F2(
			function (x, y) {
				return _elm_lang$core$Native_Utils.cmp(x, y) > -1;
			}),
		1,
		_elm_lang$core$Basics$abs(c1Tile.y - c2Tile.y)) && (A2(
		F2(
			function (x, y) {
				return _elm_lang$core$Native_Utils.cmp(x, y) > -1;
			}),
		1,
		_elm_lang$core$Basics$abs(c2Tile.x - c3Tile.x)) && (A2(
		F2(
			function (x, y) {
				return _elm_lang$core$Native_Utils.cmp(x, y) > -1;
			}),
		1,
		_elm_lang$core$Basics$abs(c2Tile.y - c3Tile.y)) && (A2(
		F2(
			function (x, y) {
				return _elm_lang$core$Native_Utils.cmp(x, y) > -1;
			}),
		1,
		_elm_lang$core$Basics$abs(c3Tile.x - c1Tile.x)) && A2(
		F2(
			function (x, y) {
				return _elm_lang$core$Native_Utils.cmp(x, y) > -1;
			}),
		1,
		_elm_lang$core$Basics$abs(c3Tile.y - c1Tile.y))))))) ? {
		ctor: '::',
		_0: c1Tile,
		_1: {
			ctor: '::',
			_0: c2Tile,
			_1: {
				ctor: '::',
				_0: c3Tile,
				_1: {ctor: '[]'}
			}
		}
	} : A2(
		_elm_lang$core$Basics_ops['++'],
		_klaftertief$elm_slippy_map$SlippyMap_Geo_Tile$triangleCover(
			{ctor: '_Tuple3', _0: c1c2Center, _1: c2c3Center, _2: c3c1Center}),
		A2(
			_elm_lang$core$Basics_ops['++'],
			_klaftertief$elm_slippy_map$SlippyMap_Geo_Tile$triangleCover(
				{ctor: '_Tuple3', _0: _p12, _1: c1c2Center, _2: c3c1Center}),
			A2(
				_elm_lang$core$Basics_ops['++'],
				_klaftertief$elm_slippy_map$SlippyMap_Geo_Tile$triangleCover(
					{ctor: '_Tuple3', _0: _p13, _1: c2c3Center, _2: c1c2Center}),
				_klaftertief$elm_slippy_map$SlippyMap_Geo_Tile$triangleCover(
					{ctor: '_Tuple3', _0: _p14, _1: c3c1Center, _2: c2c3Center}))));
};
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Tile$cover = function (_p15) {
	var _p16 = _p15;
	var _p20 = _p16.topLeft;
	var _p19 = _p16.bottomRight;
	return A2(
		_klaftertief$elm_slippy_map$SlippyMap_Geo_Tile$uniqueBy,
		function (_p17) {
			var _p18 = _p17;
			return {ctor: '_Tuple3', _0: _p18.z, _1: _p18.x, _2: _p18.y};
		},
		A2(
			_elm_lang$core$Basics_ops['++'],
			_klaftertief$elm_slippy_map$SlippyMap_Geo_Tile$triangleCover(
				{ctor: '_Tuple3', _0: _p20, _1: _p16.topRight, _2: _p19}),
			_klaftertief$elm_slippy_map$SlippyMap_Geo_Tile$triangleCover(
				{ctor: '_Tuple3', _0: _p20, _1: _p16.bottomLeft, _2: _p19})));
};
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Tile$fromComparable = function (_p21) {
	var _p22 = _p21;
	return {z: _p22._0, x: _p22._1, y: _p22._2};
};
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Tile$toComparable = function (_p23) {
	var _p24 = _p23;
	return {ctor: '_Tuple3', _0: _p24.z, _1: _p24.x, _2: _p24.y};
};
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Tile$Tile = F3(
	function (a, b, c) {
		return {z: a, x: b, y: c};
	});

var _klaftertief$elm_slippy_map$SlippyMap_Geo_Mercator$radiansToDegrees = function (rad) {
	return (rad * 180) / _elm_lang$core$Basics$pi;
};
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Mercator$degreesToRadians = function (deg) {
	return (deg * _elm_lang$core$Basics$pi) / 180;
};
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Mercator$yToLat = function (y) {
	return _klaftertief$elm_slippy_map$SlippyMap_Geo_Mercator$radiansToDegrees(
		(2 * _elm_lang$core$Basics$atan(
			Math.pow(_elm_lang$core$Basics$e, y))) - (_elm_lang$core$Basics$pi / 2));
};
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Mercator$xToLon = _klaftertief$elm_slippy_map$SlippyMap_Geo_Mercator$radiansToDegrees;
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Mercator$latToY = function (lat) {
	return A2(
		_elm_lang$core$Basics$logBase,
		_elm_lang$core$Basics$e,
		_elm_lang$core$Basics$tan(
			(_elm_lang$core$Basics$pi / 4) + (_klaftertief$elm_slippy_map$SlippyMap_Geo_Mercator$degreesToRadians(lat) / 2)));
};
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Mercator$lonToX = _klaftertief$elm_slippy_map$SlippyMap_Geo_Mercator$degreesToRadians;
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Mercator$unproject = function (_p0) {
	var _p1 = _p0;
	return {
		lon: _klaftertief$elm_slippy_map$SlippyMap_Geo_Mercator$xToLon(_p1.x),
		lat: _klaftertief$elm_slippy_map$SlippyMap_Geo_Mercator$yToLat(_p1.y)
	};
};
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Mercator$project = function (_p2) {
	var _p3 = _p2;
	return {
		x: _klaftertief$elm_slippy_map$SlippyMap_Geo_Mercator$lonToX(_p3.lon),
		y: _klaftertief$elm_slippy_map$SlippyMap_Geo_Mercator$latToY(_p3.lat)
	};
};

var _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$tileTransform = function (transform) {
	return _elm_lang$core$Native_Utils.update(
		transform,
		{
			zoom: _elm_lang$core$Basics$toFloat(
				_elm_lang$core$Basics$round(transform.zoom))
		});
};
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$scaleZoom = function (scale) {
	return A2(_elm_lang$core$Basics$logBase, _elm_lang$core$Basics$e, scale) / A2(_elm_lang$core$Basics$logBase, _elm_lang$core$Basics$e, 2);
};
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$zoomScale = function (zoom) {
	return Math.pow(2, zoom);
};
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$tileScale = function (transform) {
	return _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$zoomScale(
		transform.zoom - function (_) {
			return _.zoom;
		}(
			_klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$tileTransform(transform)));
};
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$tileZoom = function (_p0) {
	return _elm_lang$core$Basics$toFloat(
		_elm_lang$core$Basics$floor(
			function (_) {
				return _.zoom;
			}(_p0)));
};
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$relativePointToMercatorPoint = function (_p1) {
	var _p2 = _p1;
	return {x: ((_p2.x * 2) - 1) * _elm_lang$core$Basics$pi, y: (0 - ((_p2.y * 2) - 1)) * _elm_lang$core$Basics$pi};
};
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$mercatorPointToRelativePoint = function (_p3) {
	var _p4 = _p3;
	return {x: (1 + (_p4.x / _elm_lang$core$Basics$pi)) / 2, y: (1 - (_p4.y / _elm_lang$core$Basics$pi)) / 2};
};
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$relativePointToCoordinate = F2(
	function (_p6, _p5) {
		var _p7 = _p6;
		var _p9 = _p7.zoom;
		var _p8 = _p5;
		var scale = _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$zoomScale(_p9);
		return {column: _p8.x * scale, row: _p8.y * scale, zoom: _p9};
	});
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$coordinateToRelativePoint = F2(
	function (transform, coordinate) {
		var _p10 = A2(_klaftertief$elm_slippy_map$SlippyMap_Geo_Coordinate$zoomTo, transform.zoom, coordinate);
		var column = _p10.column;
		var row = _p10.row;
		var zoom = _p10.zoom;
		var scale = _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$zoomScale(zoom);
		return {x: column / scale, y: row / scale};
	});
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$coordinateToMercatorPoint = function (transform) {
	return function (_p11) {
		return _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$relativePointToMercatorPoint(
			A2(_klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$coordinateToRelativePoint, transform, _p11));
	};
};
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$mercatorPointToCoordinate = function (transform) {
	return function (_p12) {
		return A2(
			_klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$relativePointToCoordinate,
			transform,
			_klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$mercatorPointToRelativePoint(_p12));
	};
};
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$coordinateToPoint = F2(
	function (transform, coordinate) {
		var scale = _elm_lang$core$Basics$toFloat(transform.tileSize);
		var _p13 = A2(_klaftertief$elm_slippy_map$SlippyMap_Geo_Coordinate$zoomTo, transform.zoom, coordinate);
		var column = _p13.column;
		var row = _p13.row;
		var zoom = _p13.zoom;
		return {x: column * scale, y: row * scale};
	});
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$pointToCoordinate = F2(
	function (transform, _p14) {
		var _p15 = _p14;
		var scale = _elm_lang$core$Basics$toFloat(transform.tileSize);
		return {column: _p15.x / scale, row: _p15.y / scale, zoom: transform.zoom};
	});
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$coordinateToLocation = F2(
	function (transform, coordinate) {
		return _klaftertief$elm_slippy_map$SlippyMap_Geo_Mercator$unproject(
			A2(_klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$coordinateToMercatorPoint, transform, coordinate));
	});
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$locationToCoordinate = F2(
	function (transform, location) {
		return A2(
			_klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$mercatorPointToCoordinate,
			transform,
			_klaftertief$elm_slippy_map$SlippyMap_Geo_Mercator$project(location));
	});
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$pointToLocation = F2(
	function (transform, point) {
		return A2(
			_klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$coordinateToLocation,
			transform,
			A2(_klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$pointToCoordinate, transform, point));
	});
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$locationToPoint = F2(
	function (transform, location) {
		return A2(
			_klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$coordinateToPoint,
			transform,
			A2(_klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$locationToCoordinate, transform, location));
	});
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$centerPoint = function (transform) {
	return A2(_klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$locationToPoint, transform, transform.center);
};
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$scaledBounds = F2(
	function (scale, transform) {
		var center = _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$centerPoint(transform);
		var _p16 = {
			ctor: '_Tuple2',
			_0: A2(
				_klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$pointToCoordinate,
				transform,
				{x: center.x - ((transform.width / 2) / scale), y: center.y - ((transform.height / 2) / scale)}),
			_1: A2(
				_klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$pointToCoordinate,
				transform,
				{x: center.x + ((transform.width / 2) / scale), y: center.y + ((transform.height / 2) / scale)})
		};
		var topLeftCoordinate = _p16._0;
		var bottomRightCoordinate = _p16._1;
		return {
			topLeft: topLeftCoordinate,
			topRight: _elm_lang$core$Native_Utils.update(
				topLeftCoordinate,
				{column: bottomRightCoordinate.column}),
			bottomRight: bottomRightCoordinate,
			bottomLeft: _elm_lang$core$Native_Utils.update(
				topLeftCoordinate,
				{row: bottomRightCoordinate.row})
		};
	});
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$bounds = _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$scaledBounds(1);
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$tileBounds = function (transform) {
	return A2(
		_klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$scaledBounds,
		_klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$tileScale(transform),
		_klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$tileTransform(transform));
};
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$locationBounds = function (transform) {
	var center = _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$centerPoint(transform);
	var southWest = A2(
		_klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$pointToLocation,
		transform,
		{x: center.x - (transform.width / 2), y: center.y + (transform.height / 2)});
	var northEast = A2(
		_klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$pointToLocation,
		transform,
		{x: center.x + (transform.width / 2), y: center.y - (transform.height / 2)});
	return {southWest: southWest, northEast: northEast};
};
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$pixelBounds = function (transform) {
	var center = _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$centerPoint(transform);
	var topLeft = {x: center.x - (transform.width / 2), y: center.y + (transform.height / 2)};
	var bottomRight = {x: center.x + (transform.width / 2), y: center.y - (transform.height / 2)};
	return {topLeft: topLeft, bottomRight: bottomRight};
};
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$moveTo = F2(
	function (transform, toPoint) {
		var currentCenterPoint = _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$centerPoint(transform);
		var newCenterPoint = {x: (toPoint.x + currentCenterPoint.x) - (transform.width / 2), y: (toPoint.y + currentCenterPoint.y) - (transform.height / 2)};
		return _elm_lang$core$Native_Utils.update(
			transform,
			{
				center: A2(_klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$pointToLocation, transform, newCenterPoint)
			});
	});
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$zoomToAround = F3(
	function (transform, newZoom, around) {
		var currentCenterPoint = _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$centerPoint(transform);
		var aroundPoint = {x: (around.x + currentCenterPoint.x) - (transform.width / 2), y: (around.y + currentCenterPoint.y) - (transform.height / 2)};
		var aroundLocation = A2(_klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$pointToLocation, transform, aroundPoint);
		var transformZoomed = _elm_lang$core$Native_Utils.update(
			transform,
			{zoom: newZoom});
		var aroundPointZoomed = A2(_klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$locationToPoint, transformZoomed, aroundLocation);
		var aroundPointDiff = {x: aroundPointZoomed.x - aroundPoint.x, y: aroundPointZoomed.y - aroundPoint.y};
		var newCenter = A2(
			_klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$pointToLocation,
			transformZoomed,
			{x: currentCenterPoint.x + aroundPointDiff.x, y: currentCenterPoint.y + aroundPointDiff.y});
		return _elm_lang$core$Native_Utils.update(
			transform,
			{zoom: newZoom, center: newCenter});
	});
var _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$Transform = F5(
	function (a, b, c, d, e) {
		return {tileSize: a, width: b, height: c, center: d, zoom: e};
	});

var _klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$render = function (_p0) {
	var _p1 = _p0;
	return _p1._0.render;
};
var _klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$getPane = function (_p2) {
	var _p3 = _p2;
	return _p3._0.config.pane;
};
var _klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$getAttribution = function (_p4) {
	var _p5 = _p4;
	return _p5._0.config.attribution;
};
var _klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$transformToRenderState = function (transform) {
	var centerPoint = _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$centerPoint(transform);
	var size = {x: transform.width, y: transform.height};
	var halfSize = A2(_klaftertief$elm_slippy_map$SlippyMap_Geo_Point$divideBy, 2, size);
	var topLeftPoint = A2(_klaftertief$elm_slippy_map$SlippyMap_Geo_Point$subtract, halfSize, centerPoint);
	var tileTransform = _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$tileTransform(transform);
	return {
		center: transform.center,
		zoom: transform.zoom,
		bounds: _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$locationBounds(transform),
		size: size,
		halfSize: halfSize,
		pixelBounds: _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$pixelBounds(transform),
		locationToContainerPoint: function (_p6) {
			return A2(
				_klaftertief$elm_slippy_map$SlippyMap_Geo_Point$subtract,
				topLeftPoint,
				A2(_klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$locationToPoint, transform, _p6));
		},
		containerPointToLocation: function (_p7) {
			return A2(
				_klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$pointToLocation,
				transform,
				A2(_klaftertief$elm_slippy_map$SlippyMap_Geo_Point$subtract, topLeftPoint, _p7));
		},
		coordinateToContainerPoint: function (_p8) {
			return A2(
				_klaftertief$elm_slippy_map$SlippyMap_Geo_Point$subtract,
				topLeftPoint,
				A2(_klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$coordinateToPoint, transform, _p8));
		},
		transform: transform,
		tileTransform: tileTransform,
		centerPoint: centerPoint,
		tileTransformCenterPoint: _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$centerPoint(tileTransform),
		tileScale: _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$tileScale(transform),
		locationBounds: _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$locationBounds(transform),
		coordinateBounds: _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$bounds(transform),
		coordinateTileBounds: _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$tileBounds(transform),
		tileCover: _klaftertief$elm_slippy_map$SlippyMap_Geo_Tile$cover(
			_klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$tileBounds(transform))
	};
};
var _klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$ConfigInternal = F2(
	function (a, b) {
		return {attribution: a, pane: b};
	});
var _klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$LayerInternal = F2(
	function (a, b) {
		return {config: a, render: b};
	});
var _klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$RenderState = function (a) {
	return function (b) {
		return function (c) {
			return function (d) {
				return function (e) {
					return function (f) {
						return function (g) {
							return function (h) {
								return function (i) {
									return function (j) {
										return function (k) {
											return function (l) {
												return function (m) {
													return function (n) {
														return function (o) {
															return function (p) {
																return function (q) {
																	return function (r) {
																		return {center: a, zoom: b, bounds: c, size: d, halfSize: e, pixelBounds: f, locationToContainerPoint: g, containerPointToLocation: h, coordinateToContainerPoint: i, transform: j, tileTransform: k, centerPoint: l, tileTransformCenterPoint: m, tileScale: n, locationBounds: o, coordinateBounds: p, coordinateTileBounds: q, tileCover: r};
																	};
																};
															};
														};
													};
												};
											};
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var _klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$Config = function (a) {
	return {ctor: 'Config', _0: a};
};
var _klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$withAttribution = F2(
	function (attribution, _p9) {
		var _p10 = _p9;
		return _klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$Config(
			_elm_lang$core$Native_Utils.update(
				_p10._0,
				{
					attribution: _elm_lang$core$Maybe$Just(attribution)
				}));
	});
var _klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$ControlPane = {ctor: 'ControlPane'};
var _klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$MarkerPane = {ctor: 'MarkerPane'};
var _klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$marker = _klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$Config(
	{attribution: _elm_lang$core$Maybe$Nothing, pane: _klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$MarkerPane});
var _klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$isMarkerLayer = function (_p11) {
	var _p12 = _p11;
	return _elm_lang$core$Native_Utils.eq(_p12._0.config.pane, _klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$MarkerPane);
};
var _klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$OverlayPane = {ctor: 'OverlayPane'};
var _klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$overlay = _klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$Config(
	{attribution: _elm_lang$core$Maybe$Nothing, pane: _klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$OverlayPane});
var _klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$isOverlayLayer = function (_p13) {
	var _p14 = _p13;
	return _elm_lang$core$Native_Utils.eq(_p14._0.config.pane, _klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$OverlayPane);
};
var _klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$TilePane = {ctor: 'TilePane'};
var _klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$panes = {
	ctor: '::',
	_0: _klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$TilePane,
	_1: {
		ctor: '::',
		_0: _klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$OverlayPane,
		_1: {
			ctor: '::',
			_0: _klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$MarkerPane,
			_1: {
				ctor: '::',
				_0: _klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$ControlPane,
				_1: {ctor: '[]'}
			}
		}
	}
};
var _klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$tile = _klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$Config(
	{attribution: _elm_lang$core$Maybe$Nothing, pane: _klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$TilePane});
var _klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$isTileLayer = function (_p15) {
	var _p16 = _p15;
	return _elm_lang$core$Native_Utils.eq(_p16._0.config.pane, _klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$TilePane);
};
var _klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$Layer = function (a) {
	return {ctor: 'Layer', _0: a};
};
var _klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$withRender = F2(
	function (_p17, render) {
		var _p18 = _p17;
		return _klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$Layer(
			{config: _p18._0, render: render});
	});

var _klaftertief$elm_slippy_map$SlippyMap_Map_State$getTransform = function (_p0) {
	var _p1 = _p0;
	return _p1._0.transform;
};
var _klaftertief$elm_slippy_map$SlippyMap_Map_State$getLocationBounds = function (_p2) {
	return _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$locationBounds(
		_klaftertief$elm_slippy_map$SlippyMap_Map_State$getTransform(_p2));
};
var _klaftertief$elm_slippy_map$SlippyMap_Map_State$getCoordinateBounds = function (_p3) {
	return _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$tileBounds(
		_klaftertief$elm_slippy_map$SlippyMap_Map_State$getTransform(_p3));
};
var _klaftertief$elm_slippy_map$SlippyMap_Map_State$getTileCover = function (_p4) {
	return _klaftertief$elm_slippy_map$SlippyMap_Geo_Tile$cover(
		_klaftertief$elm_slippy_map$SlippyMap_Map_State$getCoordinateBounds(_p4));
};
var _klaftertief$elm_slippy_map$SlippyMap_Map_State$defaultTransform = {
	tileSize: 256,
	width: 600,
	height: 400,
	center: {lon: 0, lat: 0},
	zoom: 0
};
var _klaftertief$elm_slippy_map$SlippyMap_Map_State$Drag = F2(
	function (a, b) {
		return {last: a, current: b};
	});
var _klaftertief$elm_slippy_map$SlippyMap_Map_State$Pinch = F2(
	function (a, b) {
		return {last: a, current: b};
	});
var _klaftertief$elm_slippy_map$SlippyMap_Map_State$State = function (a) {
	return {ctor: 'State', _0: a};
};
var _klaftertief$elm_slippy_map$SlippyMap_Map_State$setTransform = F2(
	function (newTransform, _p5) {
		var _p6 = _p5;
		return _klaftertief$elm_slippy_map$SlippyMap_Map_State$State(
			_elm_lang$core$Native_Utils.update(
				_p6._0,
				{transform: newTransform}));
	});
var _klaftertief$elm_slippy_map$SlippyMap_Map_State$withDragTransform = F2(
	function (_p8, _p7) {
		var _p9 = _p8;
		var _p13 = _p9.last;
		var _p12 = _p9.current;
		var _p10 = _p7;
		var _p11 = _p10._0.transform;
		return A2(
			_klaftertief$elm_slippy_map$SlippyMap_Map_State$setTransform,
			A2(
				_klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$moveTo,
				_p11,
				{
					x: (_p11.width / 2) + _elm_lang$core$Basics$toFloat(_p13.x - _p12.x),
					y: (_p11.height / 2) + _elm_lang$core$Basics$toFloat(_p13.y - _p12.y)
				}),
			_p10);
	});
var _klaftertief$elm_slippy_map$SlippyMap_Map_State$moveTo = F2(
	function (newCenterPoint, _p14) {
		var _p15 = _p14;
		return A2(
			_klaftertief$elm_slippy_map$SlippyMap_Map_State$setTransform,
			A2(_klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$moveTo, _p15._0.transform, newCenterPoint),
			_p15);
	});
var _klaftertief$elm_slippy_map$SlippyMap_Map_State$setCenter = F2(
	function (newCenter, _p16) {
		var _p17 = _p16;
		return A2(
			_klaftertief$elm_slippy_map$SlippyMap_Map_State$setTransform,
			_elm_lang$core$Native_Utils.update(
				_p17._0.transform,
				{center: newCenter}),
			_p17);
	});
var _klaftertief$elm_slippy_map$SlippyMap_Map_State$setSize = F2(
	function (_p19, _p18) {
		var _p20 = _p19;
		var _p21 = _p18;
		return A2(
			_klaftertief$elm_slippy_map$SlippyMap_Map_State$setTransform,
			_elm_lang$core$Native_Utils.update(
				_p21._0.transform,
				{
					width: _elm_lang$core$Basics$toFloat(_p20._0),
					height: _elm_lang$core$Basics$toFloat(_p20._1)
				}),
			_p21);
	});
var _klaftertief$elm_slippy_map$SlippyMap_Map_State$setZoom = F2(
	function (newZoom, _p22) {
		var _p23 = _p22;
		var _p24 = _p23;
		return (_elm_lang$core$Basics$isNaN(newZoom) || _elm_lang$core$Basics$isInfinite(newZoom)) ? _p24 : A2(
			_klaftertief$elm_slippy_map$SlippyMap_Map_State$setTransform,
			_elm_lang$core$Native_Utils.update(
				_p23._0.transform,
				{zoom: newZoom}),
			_p24);
	});
var _klaftertief$elm_slippy_map$SlippyMap_Map_State$zoomIn = function (_p25) {
	var _p26 = _p25;
	return A2(_klaftertief$elm_slippy_map$SlippyMap_Map_State$setZoom, _p26._0.transform.zoom + 1, _p26);
};
var _klaftertief$elm_slippy_map$SlippyMap_Map_State$zoomOut = function (_p27) {
	var _p28 = _p27;
	return A2(_klaftertief$elm_slippy_map$SlippyMap_Map_State$setZoom, _p28._0.transform.zoom - 1, _p28);
};
var _klaftertief$elm_slippy_map$SlippyMap_Map_State$zoomByAround = F3(
	function (delta, point, _p29) {
		var _p30 = _p29;
		var _p31 = _p30._0.transform;
		var newZoom = (_elm_lang$core$Basics$isNaN(delta) || _elm_lang$core$Basics$isInfinite(delta)) ? _p31.zoom : (_p31.zoom + delta);
		return A2(
			_klaftertief$elm_slippy_map$SlippyMap_Map_State$setTransform,
			A3(_klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$zoomToAround, _p31, newZoom, point),
			_p30);
	});
var _klaftertief$elm_slippy_map$SlippyMap_Map_State$withPinchTransform = F2(
	function (_p33, _p32) {
		var _p34 = _p33;
		var _p42 = _p34.last;
		var _p41 = _p34.current;
		var _p35 = _p32;
		var _p40 = _p35._0.transform;
		var toPoint = function (position) {
			return {
				x: _elm_lang$core$Basics$toFloat(position.x),
				y: _elm_lang$core$Basics$toFloat(position.y)
			};
		};
		var centerPoint = function (_p36) {
			var _p37 = _p36;
			return A2(
				_klaftertief$elm_slippy_map$SlippyMap_Geo_Point$divideBy,
				2,
				A2(
					_klaftertief$elm_slippy_map$SlippyMap_Geo_Point$add,
					toPoint(_p37._0),
					toPoint(_p37._1)));
		};
		var lastCenter = centerPoint(_p42);
		var currentCenter = centerPoint(_p41);
		var newCenterPoint = {x: ((_p40.width / 2) + lastCenter.x) - currentCenter.x, y: ((_p40.height / 2) + lastCenter.y) - currentCenter.y};
		var distance = function (_p38) {
			var _p39 = _p38;
			return A2(
				_klaftertief$elm_slippy_map$SlippyMap_Geo_Point$distance,
				toPoint(_p39._0),
				toPoint(_p39._1));
		};
		var lastDistance = distance(_p42);
		var currentDistance = distance(_p41);
		var zoomDelta = _klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$scaleZoom(currentDistance / lastDistance);
		return A3(
			_klaftertief$elm_slippy_map$SlippyMap_Map_State$zoomByAround,
			zoomDelta,
			currentCenter,
			A2(_klaftertief$elm_slippy_map$SlippyMap_Map_State$moveTo, newCenterPoint, _p35));
	});
var _klaftertief$elm_slippy_map$SlippyMap_Map_State$withInteractionTransform = function (_p43) {
	var _p44 = _p43;
	var _p46 = _p44;
	var _p45 = _p44._0.interaction;
	if (_p45.ctor === 'Just') {
		if (_p45._0.ctor === 'Dragging') {
			return A2(_klaftertief$elm_slippy_map$SlippyMap_Map_State$withDragTransform, _p45._0._0, _p46);
		} else {
			return A2(_klaftertief$elm_slippy_map$SlippyMap_Map_State$withPinchTransform, _p45._0._0, _p46);
		}
	} else {
		return _p46;
	}
};
var _klaftertief$elm_slippy_map$SlippyMap_Map_State$zoomInAround = _klaftertief$elm_slippy_map$SlippyMap_Map_State$zoomByAround(1);
var _klaftertief$elm_slippy_map$SlippyMap_Map_State$setInteraction = F2(
	function (newInteraction, _p47) {
		var _p48 = _p47;
		return _klaftertief$elm_slippy_map$SlippyMap_Map_State$State(
			_elm_lang$core$Native_Utils.update(
				_p48._0,
				{interaction: newInteraction}));
	});
var _klaftertief$elm_slippy_map$SlippyMap_Map_State$setFocus = F2(
	function (newFocus, _p49) {
		var _p50 = _p49;
		return _klaftertief$elm_slippy_map$SlippyMap_Map_State$State(
			_elm_lang$core$Native_Utils.update(
				_p50._0,
				{focus: newFocus}));
	});
var _klaftertief$elm_slippy_map$SlippyMap_Map_State$Pinching = function (a) {
	return {ctor: 'Pinching', _0: a};
};
var _klaftertief$elm_slippy_map$SlippyMap_Map_State$Dragging = function (a) {
	return {ctor: 'Dragging', _0: a};
};
var _klaftertief$elm_slippy_map$SlippyMap_Map_State$HasNoFocus = {ctor: 'HasNoFocus'};
var _klaftertief$elm_slippy_map$SlippyMap_Map_State$defaultState = _klaftertief$elm_slippy_map$SlippyMap_Map_State$State(
	{transform: _klaftertief$elm_slippy_map$SlippyMap_Map_State$defaultTransform, interaction: _elm_lang$core$Maybe$Nothing, focus: _klaftertief$elm_slippy_map$SlippyMap_Map_State$HasNoFocus});
var _klaftertief$elm_slippy_map$SlippyMap_Map_State$center = F2(
	function (initialCenter, initialZoom) {
		return A2(
			_klaftertief$elm_slippy_map$SlippyMap_Map_State$setZoom,
			initialZoom,
			A2(_klaftertief$elm_slippy_map$SlippyMap_Map_State$setCenter, initialCenter, _klaftertief$elm_slippy_map$SlippyMap_Map_State$defaultState));
	});
var _klaftertief$elm_slippy_map$SlippyMap_Map_State$HasFocus = {ctor: 'HasFocus'};

var _klaftertief$elm_slippy_map$SlippyMap_Map_Msg$KeyboardNavigation = function (a) {
	return {ctor: 'KeyboardNavigation', _0: a};
};
var _klaftertief$elm_slippy_map$SlippyMap_Map_Msg$SetFocus = function (a) {
	return {ctor: 'SetFocus', _0: a};
};
var _klaftertief$elm_slippy_map$SlippyMap_Map_Msg$PinchMsg = function (a) {
	return {ctor: 'PinchMsg', _0: a};
};
var _klaftertief$elm_slippy_map$SlippyMap_Map_Msg$DragMsg = function (a) {
	return {ctor: 'DragMsg', _0: a};
};
var _klaftertief$elm_slippy_map$SlippyMap_Map_Msg$ZoomByAround = F2(
	function (a, b) {
		return {ctor: 'ZoomByAround', _0: a, _1: b};
	});
var _klaftertief$elm_slippy_map$SlippyMap_Map_Msg$ZoomInAround = function (a) {
	return {ctor: 'ZoomInAround', _0: a};
};
var _klaftertief$elm_slippy_map$SlippyMap_Map_Msg$ZoomOut = {ctor: 'ZoomOut'};
var _klaftertief$elm_slippy_map$SlippyMap_Map_Msg$ZoomIn = {ctor: 'ZoomIn'};
var _klaftertief$elm_slippy_map$SlippyMap_Map_Msg$DragEnd = function (a) {
	return {ctor: 'DragEnd', _0: a};
};
var _klaftertief$elm_slippy_map$SlippyMap_Map_Msg$DragAt = function (a) {
	return {ctor: 'DragAt', _0: a};
};
var _klaftertief$elm_slippy_map$SlippyMap_Map_Msg$DragStart = function (a) {
	return {ctor: 'DragStart', _0: a};
};
var _klaftertief$elm_slippy_map$SlippyMap_Map_Msg$PinchEnd = function (a) {
	return {ctor: 'PinchEnd', _0: a};
};
var _klaftertief$elm_slippy_map$SlippyMap_Map_Msg$PinchAt = function (a) {
	return {ctor: 'PinchAt', _0: a};
};
var _klaftertief$elm_slippy_map$SlippyMap_Map_Msg$PinchStart = function (a) {
	return {ctor: 'PinchStart', _0: a};
};

var _klaftertief$elm_slippy_map$SlippyMap_Control_Zoom$buttonSize = 24;
var _klaftertief$elm_slippy_map$SlippyMap_Control_Zoom$control = function (renderState) {
	return A2(
		_elm_lang$svg$Svg$g,
		{
			ctor: '::',
			_0: _elm_lang$svg$Svg_Attributes$transform('translate(8, 8)'),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$svg$Svg$g,
				{
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$style('cursor:pointer;'),
					_1: {
						ctor: '::',
						_0: _elm_lang$svg$Svg_Events$onClick(_klaftertief$elm_slippy_map$SlippyMap_Map_Msg$ZoomIn),
						_1: {ctor: '[]'}
					}
				},
				{
					ctor: '::',
					_0: A2(
						_elm_lang$svg$Svg$rect,
						{
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$fill('#fff'),
							_1: {
								ctor: '::',
								_0: _elm_lang$svg$Svg_Attributes$strokeWidth('1'),
								_1: {
									ctor: '::',
									_0: _elm_lang$svg$Svg_Attributes$stroke('#aaa'),
									_1: {
										ctor: '::',
										_0: _elm_lang$svg$Svg_Attributes$x('0'),
										_1: {
											ctor: '::',
											_0: _elm_lang$svg$Svg_Attributes$y('0'),
											_1: {
												ctor: '::',
												_0: _elm_lang$svg$Svg_Attributes$width(
													_elm_lang$core$Basics$toString(_klaftertief$elm_slippy_map$SlippyMap_Control_Zoom$buttonSize)),
												_1: {
													ctor: '::',
													_0: _elm_lang$svg$Svg_Attributes$height(
														_elm_lang$core$Basics$toString(_klaftertief$elm_slippy_map$SlippyMap_Control_Zoom$buttonSize)),
													_1: {ctor: '[]'}
												}
											}
										}
									}
								}
							}
						},
						{ctor: '[]'}),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$svg$Svg$path,
							{
								ctor: '::',
								_0: _elm_lang$svg$Svg_Attributes$d('M6,12L18,12M12,6L12,18'),
								_1: {
									ctor: '::',
									_0: _elm_lang$svg$Svg_Attributes$strokeWidth('2'),
									_1: {
										ctor: '::',
										_0: _elm_lang$svg$Svg_Attributes$stroke('#444'),
										_1: {ctor: '[]'}
									}
								}
							},
							{ctor: '[]'}),
						_1: {ctor: '[]'}
					}
				}),
			_1: {
				ctor: '::',
				_0: A2(
					_elm_lang$svg$Svg$g,
					{
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$transform(
							A2(
								_elm_lang$core$Basics_ops['++'],
								'translate(0, ',
								A2(
									_elm_lang$core$Basics_ops['++'],
									_elm_lang$core$Basics$toString(_klaftertief$elm_slippy_map$SlippyMap_Control_Zoom$buttonSize),
									')'))),
						_1: {
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$style('cursor:pointer;'),
							_1: {
								ctor: '::',
								_0: _elm_lang$svg$Svg_Events$onClick(_klaftertief$elm_slippy_map$SlippyMap_Map_Msg$ZoomOut),
								_1: {ctor: '[]'}
							}
						}
					},
					{
						ctor: '::',
						_0: A2(
							_elm_lang$svg$Svg$rect,
							{
								ctor: '::',
								_0: _elm_lang$svg$Svg_Attributes$fill('#fff'),
								_1: {
									ctor: '::',
									_0: _elm_lang$svg$Svg_Attributes$strokeWidth('1'),
									_1: {
										ctor: '::',
										_0: _elm_lang$svg$Svg_Attributes$stroke('#aaa'),
										_1: {
											ctor: '::',
											_0: _elm_lang$svg$Svg_Attributes$x('0'),
											_1: {
												ctor: '::',
												_0: _elm_lang$svg$Svg_Attributes$y('0'),
												_1: {
													ctor: '::',
													_0: _elm_lang$svg$Svg_Attributes$width(
														_elm_lang$core$Basics$toString(_klaftertief$elm_slippy_map$SlippyMap_Control_Zoom$buttonSize)),
													_1: {
														ctor: '::',
														_0: _elm_lang$svg$Svg_Attributes$height(
															_elm_lang$core$Basics$toString(_klaftertief$elm_slippy_map$SlippyMap_Control_Zoom$buttonSize)),
														_1: {ctor: '[]'}
													}
												}
											}
										}
									}
								}
							},
							{ctor: '[]'}),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$svg$Svg$path,
								{
									ctor: '::',
									_0: _elm_lang$svg$Svg_Attributes$d('M6,12L18,12'),
									_1: {
										ctor: '::',
										_0: _elm_lang$svg$Svg_Attributes$strokeWidth('2'),
										_1: {
											ctor: '::',
											_0: _elm_lang$svg$Svg_Attributes$stroke('#444'),
											_1: {ctor: '[]'}
										}
									}
								},
								{ctor: '[]'}),
							_1: {ctor: '[]'}
						}
					}),
				_1: {ctor: '[]'}
			}
		});
};

var _klaftertief$elm_slippy_map$SlippyMap_Map_Config$Config = function (a) {
	return {ctor: 'Config', _0: a};
};
var _klaftertief$elm_slippy_map$SlippyMap_Map_Config$staticConfig = _klaftertief$elm_slippy_map$SlippyMap_Map_Config$Config(
	{
		attributionPrefix: _elm_lang$core$Maybe$Just('Elm'),
		minZoom: 0,
		maxZoom: 19,
		toMsg: _elm_lang$core$Maybe$Nothing
	});
var _klaftertief$elm_slippy_map$SlippyMap_Map_Config$dynamicConfig = function (toMsg) {
	return _klaftertief$elm_slippy_map$SlippyMap_Map_Config$Config(
		{
			attributionPrefix: _elm_lang$core$Maybe$Just('Elm'),
			minZoom: 0,
			maxZoom: 19,
			toMsg: _elm_lang$core$Maybe$Just(toMsg)
		});
};

var _klaftertief$elm_slippy_map$SlippyMap_Map_Subscriptions$subscriptions = F2(
	function (_p1, _p0) {
		var _p2 = _p1;
		var _p3 = _p0;
		var _p4 = _p2._0.toMsg;
		if (_p4.ctor === 'Just') {
			var keyboardNavigationSubscriptions = function () {
				var _p5 = _p3._0.focus;
				if (_p5.ctor === 'HasFocus') {
					return {
						ctor: '::',
						_0: _elm_lang$keyboard$Keyboard$downs(_klaftertief$elm_slippy_map$SlippyMap_Map_Msg$KeyboardNavigation),
						_1: {ctor: '[]'}
					};
				} else {
					return {ctor: '[]'};
				}
			}();
			var dragSubscriptions = function () {
				var _p6 = _p3._0.interaction;
				if (_p6.ctor === 'Nothing') {
					return {ctor: '[]'};
				} else {
					if (_p6._0.ctor === 'Pinching') {
						return {ctor: '[]'};
					} else {
						return {
							ctor: '::',
							_0: _elm_lang$mouse$Mouse$moves(
								function (_p7) {
									return _klaftertief$elm_slippy_map$SlippyMap_Map_Msg$DragMsg(
										_klaftertief$elm_slippy_map$SlippyMap_Map_Msg$DragAt(_p7));
								}),
							_1: {
								ctor: '::',
								_0: _elm_lang$mouse$Mouse$ups(
									function (_p8) {
										return _klaftertief$elm_slippy_map$SlippyMap_Map_Msg$DragMsg(
											_klaftertief$elm_slippy_map$SlippyMap_Map_Msg$DragEnd(_p8));
									}),
								_1: {ctor: '[]'}
							}
						};
					}
				}
			}();
			return _elm_lang$core$Platform_Sub$batch(
				A2(
					_elm_lang$core$List$map,
					_elm_lang$core$Platform_Sub$map(_p4._0),
					A2(_elm_lang$core$Basics_ops['++'], dragSubscriptions, keyboardNavigationSubscriptions)));
		} else {
			return _elm_lang$core$Platform_Sub$none;
		}
	});

var _klaftertief$elm_slippy_map$SlippyMap_Map_Update$safeState = F3(
	function (_p1, oldState, _p0) {
		var _p2 = _p1;
		var _p5 = _p2._0;
		var _p3 = _p0;
		var _p4 = _p3._0.transform;
		return ((_elm_lang$core$Native_Utils.cmp(_p5.minZoom, _p4.zoom) < 1) && (_elm_lang$core$Native_Utils.cmp(_p5.maxZoom, _p4.zoom) > -1)) ? _p3 : oldState;
	});
var _klaftertief$elm_slippy_map$SlippyMap_Map_Update$updatePinch = F2(
	function (pinchMsg, _p6) {
		var _p7 = _p6;
		var _p13 = _p7;
		return _klaftertief$elm_slippy_map$SlippyMap_Map_State$withInteractionTransform(
			function () {
				var _p8 = pinchMsg;
				switch (_p8.ctor) {
					case 'PinchStart':
						var _p9 = _p8._0;
						return A2(
							_klaftertief$elm_slippy_map$SlippyMap_Map_State$setInteraction,
							_elm_lang$core$Maybe$Just(
								_klaftertief$elm_slippy_map$SlippyMap_Map_State$Pinching(
									A2(_klaftertief$elm_slippy_map$SlippyMap_Map_State$Pinch, _p9, _p9))),
							_p13);
					case 'PinchAt':
						var _p12 = _p8._0;
						var newInteraction = function () {
							var _p10 = _p7._0.interaction;
							if (_p10.ctor === 'Nothing') {
								return _elm_lang$core$Maybe$Just(
									_klaftertief$elm_slippy_map$SlippyMap_Map_State$Pinching(
										A2(_klaftertief$elm_slippy_map$SlippyMap_Map_State$Pinch, _p12, _p12)));
							} else {
								if (_p10._0.ctor === 'Dragging') {
									var _p11 = _p10._0._0.current;
									return _elm_lang$core$Maybe$Just(
										_klaftertief$elm_slippy_map$SlippyMap_Map_State$Pinching(
											A2(
												_klaftertief$elm_slippy_map$SlippyMap_Map_State$Pinch,
												{ctor: '_Tuple2', _0: _p11, _1: _p11},
												_p12)));
								} else {
									return _elm_lang$core$Maybe$Just(
										_klaftertief$elm_slippy_map$SlippyMap_Map_State$Pinching(
											A2(_klaftertief$elm_slippy_map$SlippyMap_Map_State$Pinch, _p10._0._0.current, _p12)));
								}
							}
						}();
						return A2(_klaftertief$elm_slippy_map$SlippyMap_Map_State$setInteraction, newInteraction, _p13);
					default:
						return A2(_klaftertief$elm_slippy_map$SlippyMap_Map_State$setInteraction, _elm_lang$core$Maybe$Nothing, _p13);
				}
			}());
	});
var _klaftertief$elm_slippy_map$SlippyMap_Map_Update$updateDrag = F2(
	function (dragMsg, _p14) {
		var _p15 = _p14;
		var _p20 = _p15;
		return _klaftertief$elm_slippy_map$SlippyMap_Map_State$withInteractionTransform(
			function () {
				var _p16 = dragMsg;
				switch (_p16.ctor) {
					case 'DragStart':
						var _p17 = _p16._0;
						return A2(
							_klaftertief$elm_slippy_map$SlippyMap_Map_State$setInteraction,
							_elm_lang$core$Maybe$Just(
								_klaftertief$elm_slippy_map$SlippyMap_Map_State$Dragging(
									A2(_klaftertief$elm_slippy_map$SlippyMap_Map_State$Drag, _p17, _p17))),
							_p20);
					case 'DragAt':
						var _p19 = _p16._0;
						return A2(
							_klaftertief$elm_slippy_map$SlippyMap_Map_State$setInteraction,
							A2(
								_elm_lang$core$Maybe$map,
								function (i) {
									var _p18 = i;
									if (_p18.ctor === 'Dragging') {
										return _klaftertief$elm_slippy_map$SlippyMap_Map_State$Dragging(
											A2(_klaftertief$elm_slippy_map$SlippyMap_Map_State$Drag, _p18._0.current, _p19));
									} else {
										return _klaftertief$elm_slippy_map$SlippyMap_Map_State$Dragging(
											A2(
												_klaftertief$elm_slippy_map$SlippyMap_Map_State$Drag,
												_elm_lang$core$Tuple$first(_p18._0.current),
												_p19));
									}
								},
								_p15._0.interaction),
							_p20);
					default:
						return A2(_klaftertief$elm_slippy_map$SlippyMap_Map_State$setInteraction, _elm_lang$core$Maybe$Nothing, _p20);
				}
			}());
	});
var _klaftertief$elm_slippy_map$SlippyMap_Map_Update$update = F3(
	function (config, msg, _p21) {
		var _p22 = _p21;
		var _p26 = _p22._0.transform;
		var _p25 = _p22;
		return A3(
			_klaftertief$elm_slippy_map$SlippyMap_Map_Update$safeState,
			config,
			_p25,
			function () {
				var _p23 = msg;
				switch (_p23.ctor) {
					case 'ZoomIn':
						return _klaftertief$elm_slippy_map$SlippyMap_Map_State$zoomIn(_p25);
					case 'ZoomOut':
						return _klaftertief$elm_slippy_map$SlippyMap_Map_State$zoomOut(_p25);
					case 'ZoomInAround':
						return A2(_klaftertief$elm_slippy_map$SlippyMap_Map_State$zoomInAround, _p23._0, _p25);
					case 'ZoomByAround':
						return A3(_klaftertief$elm_slippy_map$SlippyMap_Map_State$zoomByAround, _p23._0, _p23._1, _p25);
					case 'DragMsg':
						return A2(_klaftertief$elm_slippy_map$SlippyMap_Map_Update$updateDrag, _p23._0, _p25);
					case 'PinchMsg':
						return A2(_klaftertief$elm_slippy_map$SlippyMap_Map_Update$updatePinch, _p23._0, _p25);
					case 'SetFocus':
						return A2(_klaftertief$elm_slippy_map$SlippyMap_Map_State$setFocus, _p23._0, _p25);
					default:
						var offset = 50;
						var moveBy = function () {
							var _p24 = _p23._0;
							switch (_p24) {
								case 37:
									return {x: 0 - offset, y: 0};
								case 38:
									return {x: 0, y: 0 - offset};
								case 39:
									return {x: offset, y: 0};
								case 40:
									return {x: 0, y: offset};
								default:
									return {x: 0, y: 0};
							}
						}();
						return A2(
							_klaftertief$elm_slippy_map$SlippyMap_Map_State$setTransform,
							A2(
								_klaftertief$elm_slippy_map$SlippyMap_Geo_Transform$moveTo,
								_p26,
								{x: (_p26.width / 2) + moveBy.x, y: (_p26.height / 2) + moveBy.y}),
							_p25);
				}
			}());
	});

var _klaftertief$elm_slippy_map$SlippyMap_Map_View$tapDecoderAt = function (index) {
	return A3(
		_elm_lang$core$Json_Decode$map2,
		_elm_lang$mouse$Mouse$Position,
		A2(
			_elm_lang$core$Json_Decode$at,
			{
				ctor: '::',
				_0: 'targetTouches',
				_1: {
					ctor: '::',
					_0: _elm_lang$core$Basics$toString(index),
					_1: {
						ctor: '::',
						_0: 'clientX',
						_1: {ctor: '[]'}
					}
				}
			},
			_elm_lang$core$Json_Decode$int),
		A2(
			_elm_lang$core$Json_Decode$at,
			{
				ctor: '::',
				_0: 'targetTouches',
				_1: {
					ctor: '::',
					_0: _elm_lang$core$Basics$toString(index),
					_1: {
						ctor: '::',
						_0: 'clientY',
						_1: {ctor: '[]'}
					}
				}
			},
			_elm_lang$core$Json_Decode$int));
};
var _klaftertief$elm_slippy_map$SlippyMap_Map_View$pinchDecoder = A3(
	_elm_lang$core$Json_Decode$map2,
	F2(
		function (v0, v1) {
			return {ctor: '_Tuple2', _0: v0, _1: v1};
		}),
	_klaftertief$elm_slippy_map$SlippyMap_Map_View$tapDecoderAt(0),
	_klaftertief$elm_slippy_map$SlippyMap_Map_View$tapDecoderAt(1));
var _klaftertief$elm_slippy_map$SlippyMap_Map_View$tapDecoder = _klaftertief$elm_slippy_map$SlippyMap_Map_View$tapDecoderAt(0);
var _klaftertief$elm_slippy_map$SlippyMap_Map_View$touchesEndMsg = function (touches) {
	var _p0 = touches;
	if (_p0.ctor === 'Tap') {
		return _klaftertief$elm_slippy_map$SlippyMap_Map_Msg$DragMsg(
			_klaftertief$elm_slippy_map$SlippyMap_Map_Msg$DragEnd(_p0._0));
	} else {
		return _klaftertief$elm_slippy_map$SlippyMap_Map_Msg$PinchMsg(
			_klaftertief$elm_slippy_map$SlippyMap_Map_Msg$PinchEnd(_p0._0));
	}
};
var _klaftertief$elm_slippy_map$SlippyMap_Map_View$touchesMoveMsg = function (touches) {
	var _p1 = touches;
	if (_p1.ctor === 'Tap') {
		return _klaftertief$elm_slippy_map$SlippyMap_Map_Msg$DragMsg(
			_klaftertief$elm_slippy_map$SlippyMap_Map_Msg$DragAt(_p1._0));
	} else {
		return _klaftertief$elm_slippy_map$SlippyMap_Map_Msg$PinchMsg(
			_klaftertief$elm_slippy_map$SlippyMap_Map_Msg$PinchAt(_p1._0));
	}
};
var _klaftertief$elm_slippy_map$SlippyMap_Map_View$touchesStartMsg = function (touches) {
	var _p2 = touches;
	if (_p2.ctor === 'Tap') {
		return _klaftertief$elm_slippy_map$SlippyMap_Map_Msg$DragMsg(
			_klaftertief$elm_slippy_map$SlippyMap_Map_Msg$DragStart(_p2._0));
	} else {
		return _klaftertief$elm_slippy_map$SlippyMap_Map_Msg$PinchMsg(
			_klaftertief$elm_slippy_map$SlippyMap_Map_Msg$PinchStart(_p2._0));
	}
};
var _klaftertief$elm_slippy_map$SlippyMap_Map_View$clientPosition = A3(
	_elm_lang$core$Json_Decode$map2,
	_klaftertief$elm_slippy_map$SlippyMap_Geo_Point$Point,
	A2(_elm_lang$core$Json_Decode$field, 'offsetX', _elm_lang$core$Json_Decode$float),
	A2(_elm_lang$core$Json_Decode$field, 'offsetY', _elm_lang$core$Json_Decode$float));
var _klaftertief$elm_slippy_map$SlippyMap_Map_View$viewPane = F4(
	function (_p3, renderState, layers, pane) {
		var _p4 = _p3;
		return A2(
			_elm_lang$core$List$map,
			function (layer) {
				return A2(_klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$render, layer, renderState);
			},
			A2(
				_elm_lang$core$List$filter,
				function (_p5) {
					return A2(
						F2(
							function (x, y) {
								return _elm_lang$core$Native_Utils.eq(x, y);
							}),
						pane,
						_klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$getPane(_p5));
				},
				layers));
	});
var _klaftertief$elm_slippy_map$SlippyMap_Map_View$Pinch = function (a) {
	return {ctor: 'Pinch', _0: a};
};
var _klaftertief$elm_slippy_map$SlippyMap_Map_View$Tap = function (a) {
	return {ctor: 'Tap', _0: a};
};
var _klaftertief$elm_slippy_map$SlippyMap_Map_View$touchesDecoder = _elm_lang$core$Json_Decode$oneOf(
	{
		ctor: '::',
		_0: A2(_elm_lang$core$Json_Decode$map, _klaftertief$elm_slippy_map$SlippyMap_Map_View$Pinch, _klaftertief$elm_slippy_map$SlippyMap_Map_View$pinchDecoder),
		_1: {
			ctor: '::',
			_0: A2(_elm_lang$core$Json_Decode$map, _klaftertief$elm_slippy_map$SlippyMap_Map_View$Tap, _klaftertief$elm_slippy_map$SlippyMap_Map_View$tapDecoder),
			_1: {ctor: '[]'}
		}
	});
var _klaftertief$elm_slippy_map$SlippyMap_Map_View$view = F3(
	function (_p7, _p6, layers) {
		var _p8 = _p7;
		var _p15 = _p8._0;
		var _p9 = _p6;
		var _p14 = _p9._0.transform;
		var _p13 = _p9._0.interaction;
		var handlers = function () {
			var _p10 = _p15.toMsg;
			if (_p10.ctor === 'Just') {
				return A2(
					_elm_lang$core$List$map,
					_elm_lang$html$Html_Attributes$map(_p10._0),
					{
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html_Events$on,
							'dblclick',
							A2(_elm_lang$core$Json_Decode$map, _klaftertief$elm_slippy_map$SlippyMap_Map_Msg$ZoomInAround, _klaftertief$elm_slippy_map$SlippyMap_Map_View$clientPosition)),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html_Events$on,
								'mousedown',
								A2(
									_elm_lang$core$Json_Decode$map,
									function (_p11) {
										return _klaftertief$elm_slippy_map$SlippyMap_Map_Msg$DragMsg(
											_klaftertief$elm_slippy_map$SlippyMap_Map_Msg$DragStart(_p11));
									},
									_elm_lang$mouse$Mouse$position)),
							_1: {
								ctor: '::',
								_0: A3(
									_elm_lang$html$Html_Events$onWithOptions,
									'touchstart',
									{preventDefault: true, stopPropagation: true},
									A2(_elm_lang$core$Json_Decode$map, _klaftertief$elm_slippy_map$SlippyMap_Map_View$touchesStartMsg, _klaftertief$elm_slippy_map$SlippyMap_Map_View$touchesDecoder)),
								_1: {
									ctor: '::',
									_0: A2(
										_elm_lang$html$Html_Events$on,
										'touchmove',
										A2(_elm_lang$core$Json_Decode$map, _klaftertief$elm_slippy_map$SlippyMap_Map_View$touchesMoveMsg, _klaftertief$elm_slippy_map$SlippyMap_Map_View$touchesDecoder)),
									_1: {
										ctor: '::',
										_0: A2(
											_elm_lang$html$Html_Events$on,
											'touchend',
											A2(
												_elm_lang$core$Json_Decode$map,
												_klaftertief$elm_slippy_map$SlippyMap_Map_View$touchesEndMsg,
												_elm_lang$core$Json_Decode$succeed(
													_klaftertief$elm_slippy_map$SlippyMap_Map_View$Tap(
														A2(_elm_lang$mouse$Mouse$Position, 0, 0))))),
										_1: {
											ctor: '::',
											_0: A2(
												_elm_lang$html$Html_Events$on,
												'touchcancel',
												A2(
													_elm_lang$core$Json_Decode$map,
													_klaftertief$elm_slippy_map$SlippyMap_Map_View$touchesEndMsg,
													_elm_lang$core$Json_Decode$succeed(
														_klaftertief$elm_slippy_map$SlippyMap_Map_View$Tap(
															A2(_elm_lang$mouse$Mouse$Position, 0, 0))))),
											_1: {
												ctor: '::',
												_0: A3(
													_elm_lang$html$Html_Events$onWithOptions,
													'wheel',
													{preventDefault: true, stopPropagation: true},
													A3(
														_elm_lang$core$Json_Decode$map2,
														F2(
															function (offset, point) {
																return A2(_klaftertief$elm_slippy_map$SlippyMap_Map_Msg$ZoomByAround, offset, point);
															}),
														A2(
															_elm_lang$core$Json_Decode$map,
															function (y) {
																return (0 - y) / 100;
															},
															A2(_elm_lang$core$Json_Decode$field, 'deltaY', _elm_lang$core$Json_Decode$float)),
														_klaftertief$elm_slippy_map$SlippyMap_Map_View$clientPosition)),
												_1: {
													ctor: '::',
													_0: _elm_lang$html$Html_Events$onFocus(
														_klaftertief$elm_slippy_map$SlippyMap_Map_Msg$SetFocus(_klaftertief$elm_slippy_map$SlippyMap_Map_State$HasFocus)),
													_1: {
														ctor: '::',
														_0: _elm_lang$html$Html_Events$onBlur(
															_klaftertief$elm_slippy_map$SlippyMap_Map_Msg$SetFocus(_klaftertief$elm_slippy_map$SlippyMap_Map_State$HasNoFocus)),
														_1: {ctor: '[]'}
													}
												}
											}
										}
									}
								}
							}
						}
					});
			} else {
				return {ctor: '[]'};
			}
		}();
		var layerAttributions = A2(
			_elm_lang$core$List$filterMap,
			_elm_lang$core$Basics$identity,
			A2(_elm_lang$core$List$map, _klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$getAttribution, layers));
		var renderState = _klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$transformToRenderState(_p14);
		return A2(
			_elm_lang$html$Html$div,
			A2(
				_elm_lang$core$Basics_ops['++'],
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$tabindex(0),
					_1: {
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$style(
							{
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 'position', _1: 'relative'},
								_1: {
									ctor: '::',
									_0: {
										ctor: '_Tuple2',
										_0: 'width',
										_1: A2(
											_elm_lang$core$Basics_ops['++'],
											_elm_lang$core$Basics$toString(_p14.width),
											'px')
									},
									_1: {
										ctor: '::',
										_0: {
											ctor: '_Tuple2',
											_0: 'height',
											_1: A2(
												_elm_lang$core$Basics_ops['++'],
												_elm_lang$core$Basics$toString(_p14.height),
												'px')
										},
										_1: {
											ctor: '::',
											_0: {ctor: '_Tuple2', _0: 'background', _1: '#eee'},
											_1: {ctor: '[]'}
										}
									}
								}
							}),
						_1: {
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$classList(
								{
									ctor: '::',
									_0: {
										ctor: '_Tuple2',
										_0: 'with-interaction',
										_1: !_elm_lang$core$Native_Utils.eq(_p13, _elm_lang$core$Maybe$Nothing)
									},
									_1: {ctor: '[]'}
								}),
							_1: {ctor: '[]'}
						}
					}
				},
				handlers),
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$div,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$style(
							{
								ctor: '::',
								_0: {
									ctor: '_Tuple2',
									_0: 'position',
									_1: (!_elm_lang$core$Native_Utils.eq(_p13, _elm_lang$core$Maybe$Nothing)) ? 'fixed' : 'absolute'
								},
								_1: {
									ctor: '::',
									_0: {ctor: '_Tuple2', _0: 'left', _1: '0'},
									_1: {
										ctor: '::',
										_0: {ctor: '_Tuple2', _0: 'top', _1: '0'},
										_1: {
											ctor: '::',
											_0: {ctor: '_Tuple2', _0: 'right', _1: '0'},
											_1: {
												ctor: '::',
												_0: {ctor: '_Tuple2', _0: 'bottom', _1: '0'},
												_1: {ctor: '[]'}
											}
										}
									}
								}
							}),
						_1: {ctor: '[]'}
					},
					{ctor: '[]'}),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$svg$Svg$svg,
						{
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$class('esm__map'),
							_1: {
								ctor: '::',
								_0: _elm_lang$svg$Svg_Attributes$height(
									_elm_lang$core$Basics$toString(_p14.height)),
								_1: {
									ctor: '::',
									_0: _elm_lang$svg$Svg_Attributes$width(
										_elm_lang$core$Basics$toString(_p14.width)),
									_1: {
										ctor: '::',
										_0: _elm_lang$svg$Svg_Attributes$style('position: absolute;'),
										_1: {ctor: '[]'}
									}
								}
							}
						},
						{
							ctor: '::',
							_0: A2(
								_elm_lang$svg$Svg$g,
								{
									ctor: '::',
									_0: _elm_lang$svg$Svg_Attributes$class('esm__layers'),
									_1: {ctor: '[]'}
								},
								A2(
									_elm_lang$core$List$concatMap,
									A3(
										_klaftertief$elm_slippy_map$SlippyMap_Map_View$viewPane,
										_klaftertief$elm_slippy_map$SlippyMap_Map_Config$Config(_p15),
										renderState,
										layers),
									_klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$panes)),
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$div,
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$style(
									{
										ctor: '::',
										_0: {ctor: '_Tuple2', _0: 'position', _1: 'absolute'},
										_1: {
											ctor: '::',
											_0: {ctor: '_Tuple2', _0: 'left', _1: '0'},
											_1: {
												ctor: '::',
												_0: {ctor: '_Tuple2', _0: 'top', _1: '0'},
												_1: {
													ctor: '::',
													_0: {ctor: '_Tuple2', _0: 'right', _1: '0'},
													_1: {
														ctor: '::',
														_0: {ctor: '_Tuple2', _0: 'bottom', _1: '0'},
														_1: {ctor: '[]'}
													}
												}
											}
										}
									}),
								_1: {ctor: '[]'}
							},
							{ctor: '[]'}),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$svg$Svg$svg,
								{
									ctor: '::',
									_0: _elm_lang$svg$Svg_Attributes$class('esm__controls'),
									_1: {
										ctor: '::',
										_0: _elm_lang$svg$Svg_Attributes$height(
											_elm_lang$core$Basics$toString(_p14.height)),
										_1: {
											ctor: '::',
											_0: _elm_lang$svg$Svg_Attributes$width(
												_elm_lang$core$Basics$toString(_p14.width)),
											_1: {
												ctor: '::',
												_0: _elm_lang$svg$Svg_Attributes$style('position: absolute;'),
												_1: {ctor: '[]'}
											}
										}
									}
								},
								{
									ctor: '::',
									_0: A3(
										_klaftertief$elm_slippy_map$SlippyMap_Control_Attribution$control,
										{ctor: '_Tuple2', _0: _p14.width, _1: _p14.height},
										_p15.attributionPrefix,
										layerAttributions),
									_1: {
										ctor: '::',
										_0: function () {
											var _p12 = _p15.toMsg;
											if (_p12.ctor === 'Just') {
												return A2(
													_elm_lang$svg$Svg$map,
													_p12._0,
													_klaftertief$elm_slippy_map$SlippyMap_Control_Zoom$control(renderState));
											} else {
												return _elm_lang$svg$Svg$text('');
											}
										}(),
										_1: {ctor: '[]'}
									}
								}),
							_1: {ctor: '[]'}
						}
					}
				}
			});
	});

var _klaftertief$elm_slippy_map$SlippyMap_Interactive$view = F2(
	function (_p1, _p0) {
		var _p2 = _p1;
		var _p3 = _p0;
		return A2(_klaftertief$elm_slippy_map$SlippyMap_Map_View$view, _p2._0, _p3._0);
	});
var _klaftertief$elm_slippy_map$SlippyMap_Interactive$subscriptions = F2(
	function (_p5, _p4) {
		var _p6 = _p5;
		var _p7 = _p4;
		return A2(_klaftertief$elm_slippy_map$SlippyMap_Map_Subscriptions$subscriptions, _p6._0, _p7._0);
	});
var _klaftertief$elm_slippy_map$SlippyMap_Interactive$renderState = function (_p8) {
	var _p9 = _p8;
	return _klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$transformToRenderState(_p9._0._0.transform);
};
var _klaftertief$elm_slippy_map$SlippyMap_Interactive$Config = function (a) {
	return {ctor: 'Config', _0: a};
};
var _klaftertief$elm_slippy_map$SlippyMap_Interactive$State = function (a) {
	return {ctor: 'State', _0: a};
};
var _klaftertief$elm_slippy_map$SlippyMap_Interactive$center = F2(
	function (initialCenter, initialZoom) {
		return _klaftertief$elm_slippy_map$SlippyMap_Interactive$State(
			A2(
				_klaftertief$elm_slippy_map$SlippyMap_Map_State$setZoom,
				initialZoom,
				A2(_klaftertief$elm_slippy_map$SlippyMap_Map_State$setCenter, initialCenter, _klaftertief$elm_slippy_map$SlippyMap_Map_State$defaultState)));
	});
var _klaftertief$elm_slippy_map$SlippyMap_Interactive$resize = F2(
	function (dimensions, _p10) {
		var _p11 = _p10;
		return _klaftertief$elm_slippy_map$SlippyMap_Interactive$State(
			A2(_klaftertief$elm_slippy_map$SlippyMap_Map_State$setSize, dimensions, _p11._0));
	});
var _klaftertief$elm_slippy_map$SlippyMap_Interactive$update = F3(
	function (_p14, _p13, _p12) {
		var _p15 = _p14;
		var _p16 = _p13;
		var _p17 = _p12;
		return _klaftertief$elm_slippy_map$SlippyMap_Interactive$State(
			A3(_klaftertief$elm_slippy_map$SlippyMap_Map_Update$update, _p15._0, _p16._0, _p17._0));
	});
var _klaftertief$elm_slippy_map$SlippyMap_Interactive$Msg = function (a) {
	return {ctor: 'Msg', _0: a};
};
var _klaftertief$elm_slippy_map$SlippyMap_Interactive$config = function (toMsg) {
	return _klaftertief$elm_slippy_map$SlippyMap_Interactive$Config(
		_klaftertief$elm_slippy_map$SlippyMap_Map_Config$dynamicConfig(
			function (_p18) {
				return toMsg(
					_klaftertief$elm_slippy_map$SlippyMap_Interactive$Msg(_p18));
			}));
};

var _mgold$elm_geojson$GeoJson$encodePosition = function (_p0) {
	var _p1 = _p0;
	var _p4 = _p1._2;
	var _p3 = _p1._1;
	var _p2 = _p1._0;
	var coordinates = _elm_lang$core$Native_Utils.eq(_p4, 0) ? {
		ctor: '::',
		_0: _p2,
		_1: {
			ctor: '::',
			_0: _p3,
			_1: {ctor: '[]'}
		}
	} : {
		ctor: '::',
		_0: _p2,
		_1: {
			ctor: '::',
			_0: _p3,
			_1: {
				ctor: '::',
				_0: _p4,
				_1: {ctor: '[]'}
			}
		}
	};
	return _elm_lang$core$Json_Encode$list(
		A2(_elm_lang$core$List$map, _elm_lang$core$Json_Encode$float, coordinates));
};
var _mgold$elm_geojson$GeoJson$encodeBbox = function (bbox) {
	return A2(
		_elm_lang$core$Maybe$withDefault,
		{ctor: '[]'},
		A2(
			_elm_lang$core$Maybe$map,
			function (b) {
				return {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'bbox',
						_1: _elm_lang$core$Json_Encode$list(
							A2(_elm_lang$core$List$map, _elm_lang$core$Json_Encode$float, b))
					},
					_1: {ctor: '[]'}
				};
			},
			bbox));
};
var _mgold$elm_geojson$GeoJson$encodeGeometry = function (geom) {
	var _p5 = geom;
	switch (_p5.ctor) {
		case 'Point':
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'type',
					_1: _elm_lang$core$Json_Encode$string('Point')
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'coordinates',
						_1: _mgold$elm_geojson$GeoJson$encodePosition(_p5._0)
					},
					_1: {ctor: '[]'}
				}
			};
		case 'MultiPoint':
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'type',
					_1: _elm_lang$core$Json_Encode$string('MultiPoint')
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'coordinates',
						_1: _elm_lang$core$Json_Encode$list(
							A2(_elm_lang$core$List$map, _mgold$elm_geojson$GeoJson$encodePosition, _p5._0))
					},
					_1: {ctor: '[]'}
				}
			};
		case 'LineString':
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'type',
					_1: _elm_lang$core$Json_Encode$string('LineString')
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'coordinates',
						_1: _elm_lang$core$Json_Encode$list(
							A2(_elm_lang$core$List$map, _mgold$elm_geojson$GeoJson$encodePosition, _p5._0))
					},
					_1: {ctor: '[]'}
				}
			};
		case 'MultiLineString':
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'type',
					_1: _elm_lang$core$Json_Encode$string('MultiLineString')
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'coordinates',
						_1: _elm_lang$core$Json_Encode$list(
							A2(
								_elm_lang$core$List$map,
								function (_p6) {
									return _elm_lang$core$Json_Encode$list(
										A2(_elm_lang$core$List$map, _mgold$elm_geojson$GeoJson$encodePosition, _p6));
								},
								_p5._0))
					},
					_1: {ctor: '[]'}
				}
			};
		case 'Polygon':
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'type',
					_1: _elm_lang$core$Json_Encode$string('Polygon')
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'coordinates',
						_1: _elm_lang$core$Json_Encode$list(
							A2(
								_elm_lang$core$List$map,
								function (_p7) {
									return _elm_lang$core$Json_Encode$list(
										A2(_elm_lang$core$List$map, _mgold$elm_geojson$GeoJson$encodePosition, _p7));
								},
								_p5._0))
					},
					_1: {ctor: '[]'}
				}
			};
		case 'MultiPolygon':
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'type',
					_1: _elm_lang$core$Json_Encode$string('MultiPolygon')
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'coordinates',
						_1: _elm_lang$core$Json_Encode$list(
							A2(
								_elm_lang$core$List$map,
								function (_p8) {
									return _elm_lang$core$Json_Encode$list(
										A2(
											_elm_lang$core$List$map,
											function (_p9) {
												return _elm_lang$core$Json_Encode$list(
													A2(_elm_lang$core$List$map, _mgold$elm_geojson$GeoJson$encodePosition, _p9));
											},
											_p8));
								},
								_p5._0))
					},
					_1: {ctor: '[]'}
				}
			};
		default:
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'type',
					_1: _elm_lang$core$Json_Encode$string('GeometryCollection')
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'geometries',
						_1: _elm_lang$core$Json_Encode$list(
							A2(
								_elm_lang$core$List$map,
								function (_p10) {
									return _elm_lang$core$Json_Encode$object(
										_mgold$elm_geojson$GeoJson$encodeGeometry(_p10));
								},
								_p5._0))
					},
					_1: {ctor: '[]'}
				}
			};
	}
};
var _mgold$elm_geojson$GeoJson$encodeFeature = function (_p11) {
	var _p12 = _p11;
	var encodedId = function () {
		var _p13 = _p12.id;
		if (_p13.ctor === 'Nothing') {
			return {ctor: '[]'};
		} else {
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'id',
					_1: _elm_lang$core$Json_Encode$string(_p13._0)
				},
				_1: {ctor: '[]'}
			};
		}
	}();
	return A2(
		_elm_lang$core$Basics_ops['++'],
		{
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: 'type',
				_1: _elm_lang$core$Json_Encode$string('Feature')
			},
			_1: {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'geometry',
					_1: A2(
						_elm_lang$core$Maybe$withDefault,
						_elm_lang$core$Json_Encode$null,
						A2(
							_elm_lang$core$Maybe$map,
							function (_p14) {
								return _elm_lang$core$Json_Encode$object(
									_mgold$elm_geojson$GeoJson$encodeGeometry(_p14));
							},
							_p12.geometry))
				},
				_1: {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: 'properties', _1: _p12.properties},
					_1: {ctor: '[]'}
				}
			}
		},
		encodedId);
};
var _mgold$elm_geojson$GeoJson$encodeGeoJson = function (geojson) {
	var _p15 = geojson;
	switch (_p15.ctor) {
		case 'Feature':
			return _mgold$elm_geojson$GeoJson$encodeFeature(_p15._0);
		case 'FeatureCollection':
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'type',
					_1: _elm_lang$core$Json_Encode$string('FeatureCollection')
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'features',
						_1: _elm_lang$core$Json_Encode$list(
							A2(
								_elm_lang$core$List$map,
								function (_p16) {
									return _elm_lang$core$Json_Encode$object(
										_mgold$elm_geojson$GeoJson$encodeFeature(_p16));
								},
								_p15._0))
					},
					_1: {ctor: '[]'}
				}
			};
		default:
			return _mgold$elm_geojson$GeoJson$encodeGeometry(_p15._0);
	}
};
var _mgold$elm_geojson$GeoJson$encode = function (_p17) {
	var _p18 = _p17;
	return _elm_lang$core$Json_Encode$object(
		A2(
			_elm_lang$core$Basics_ops['++'],
			_mgold$elm_geojson$GeoJson$encodeGeoJson(_p18._0),
			_mgold$elm_geojson$GeoJson$encodeBbox(_p18._1)));
};
var _mgold$elm_geojson$GeoJson$decodePosition = function () {
	var errorString = function (adj) {
		return A2(
			_elm_lang$core$Basics_ops['++'],
			'Array has too ',
			A2(_elm_lang$core$Basics_ops['++'], adj, ' numbers to make a position'));
	};
	var listToTuple = function (ps) {
		var _p19 = ps;
		if (_p19.ctor === '[]') {
			return _elm_lang$core$Json_Decode$fail(
				errorString('few'));
		} else {
			if (_p19._1.ctor === '[]') {
				return _elm_lang$core$Json_Decode$fail(
					errorString('few'));
			} else {
				if (_p19._1._1.ctor === '[]') {
					return _elm_lang$core$Json_Decode$succeed(
						{ctor: '_Tuple3', _0: _p19._0, _1: _p19._1._0, _2: 0});
				} else {
					if (_p19._1._1._1.ctor === '[]') {
						return _elm_lang$core$Json_Decode$succeed(
							{ctor: '_Tuple3', _0: _p19._0, _1: _p19._1._0, _2: _p19._1._1._0});
					} else {
						return _elm_lang$core$Json_Decode$fail(
							errorString('many'));
					}
				}
			}
		}
	};
	return A2(
		_elm_lang$core$Json_Decode$andThen,
		listToTuple,
		_elm_lang$core$Json_Decode$list(_elm_lang$core$Json_Decode$float));
}();
var _mgold$elm_geojson$GeoJson$decodeBbox = _elm_lang$core$Json_Decode$list(_elm_lang$core$Json_Decode$float);
var _mgold$elm_geojson$GeoJson$FeatureObject = F3(
	function (a, b, c) {
		return {geometry: a, properties: b, id: c};
	});
var _mgold$elm_geojson$GeoJson$FeatureCollection = function (a) {
	return {ctor: 'FeatureCollection', _0: a};
};
var _mgold$elm_geojson$GeoJson$Feature = function (a) {
	return {ctor: 'Feature', _0: a};
};
var _mgold$elm_geojson$GeoJson$Geometry = function (a) {
	return {ctor: 'Geometry', _0: a};
};
var _mgold$elm_geojson$GeoJson$GeometryCollection = function (a) {
	return {ctor: 'GeometryCollection', _0: a};
};
var _mgold$elm_geojson$GeoJson$MultiPolygon = function (a) {
	return {ctor: 'MultiPolygon', _0: a};
};
var _mgold$elm_geojson$GeoJson$Polygon = function (a) {
	return {ctor: 'Polygon', _0: a};
};
var _mgold$elm_geojson$GeoJson$MultiLineString = function (a) {
	return {ctor: 'MultiLineString', _0: a};
};
var _mgold$elm_geojson$GeoJson$LineString = function (a) {
	return {ctor: 'LineString', _0: a};
};
var _mgold$elm_geojson$GeoJson$MultiPoint = function (a) {
	return {ctor: 'MultiPoint', _0: a};
};
var _mgold$elm_geojson$GeoJson$Point = function (a) {
	return {ctor: 'Point', _0: a};
};
var _mgold$elm_geojson$GeoJson$decodeGeometry = function () {
	var helper = function (tipe) {
		var _p20 = tipe;
		switch (_p20) {
			case 'Point':
				return A2(
					_elm_lang$core$Json_Decode$map,
					_mgold$elm_geojson$GeoJson$Point,
					A2(_elm_lang$core$Json_Decode$field, 'coordinates', _mgold$elm_geojson$GeoJson$decodePosition));
			case 'MultiPoint':
				return A2(
					_elm_lang$core$Json_Decode$map,
					_mgold$elm_geojson$GeoJson$MultiPoint,
					A2(
						_elm_lang$core$Json_Decode$field,
						'coordinates',
						_elm_lang$core$Json_Decode$list(_mgold$elm_geojson$GeoJson$decodePosition)));
			case 'LineString':
				return A2(
					_elm_lang$core$Json_Decode$map,
					_mgold$elm_geojson$GeoJson$LineString,
					A2(
						_elm_lang$core$Json_Decode$field,
						'coordinates',
						_elm_lang$core$Json_Decode$list(_mgold$elm_geojson$GeoJson$decodePosition)));
			case 'MultiLineString':
				return A2(
					_elm_lang$core$Json_Decode$map,
					_mgold$elm_geojson$GeoJson$MultiLineString,
					A2(
						_elm_lang$core$Json_Decode$field,
						'coordinates',
						_elm_lang$core$Json_Decode$list(
							_elm_lang$core$Json_Decode$list(_mgold$elm_geojson$GeoJson$decodePosition))));
			case 'Polygon':
				return A2(
					_elm_lang$core$Json_Decode$map,
					_mgold$elm_geojson$GeoJson$Polygon,
					A2(
						_elm_lang$core$Json_Decode$field,
						'coordinates',
						_elm_lang$core$Json_Decode$list(
							_elm_lang$core$Json_Decode$list(_mgold$elm_geojson$GeoJson$decodePosition))));
			case 'MultiPolygon':
				return A2(
					_elm_lang$core$Json_Decode$map,
					_mgold$elm_geojson$GeoJson$MultiPolygon,
					A2(
						_elm_lang$core$Json_Decode$field,
						'coordinates',
						_elm_lang$core$Json_Decode$list(
							_elm_lang$core$Json_Decode$list(
								_elm_lang$core$Json_Decode$list(_mgold$elm_geojson$GeoJson$decodePosition)))));
			case 'GeometryCollection':
				return A2(
					_elm_lang$core$Json_Decode$map,
					_mgold$elm_geojson$GeoJson$GeometryCollection,
					A2(
						_elm_lang$core$Json_Decode$field,
						'geometries',
						_elm_lang$core$Json_Decode$list(_mgold$elm_geojson$GeoJson$decodeGeometry)));
			default:
				return _elm_lang$core$Json_Decode$fail(
					A2(_elm_lang$core$Basics_ops['++'], 'Unrecognized \'type\': ', tipe));
		}
	};
	return A2(
		_elm_lang$core$Json_Decode$andThen,
		helper,
		A2(_elm_lang$core$Json_Decode$field, 'type', _elm_lang$core$Json_Decode$string));
}();
var _mgold$elm_geojson$GeoJson$decodeFeature = A4(
	_elm_lang$core$Json_Decode$map3,
	_mgold$elm_geojson$GeoJson$FeatureObject,
	A2(
		_elm_lang$core$Json_Decode$field,
		'geometry',
		_elm_lang$core$Json_Decode$oneOf(
			{
				ctor: '::',
				_0: _elm_lang$core$Json_Decode$null(_elm_lang$core$Maybe$Nothing),
				_1: {
					ctor: '::',
					_0: A2(_elm_lang$core$Json_Decode$map, _elm_lang$core$Maybe$Just, _mgold$elm_geojson$GeoJson$decodeGeometry),
					_1: {ctor: '[]'}
				}
			})),
	A2(_elm_lang$core$Json_Decode$field, 'properties', _elm_lang$core$Json_Decode$value),
	_elm_lang$core$Json_Decode$maybe(
		A2(
			_elm_lang$core$Json_Decode$field,
			'id',
			_elm_lang$core$Json_Decode$oneOf(
				{
					ctor: '::',
					_0: _elm_lang$core$Json_Decode$string,
					_1: {
						ctor: '::',
						_0: A2(_elm_lang$core$Json_Decode$map, _elm_lang$core$Basics$toString, _elm_lang$core$Json_Decode$int),
						_1: {ctor: '[]'}
					}
				}))));
var _mgold$elm_geojson$GeoJson$decodeGeoJson = function (tipe) {
	var helper = function (tipe) {
		var _p21 = tipe;
		switch (_p21) {
			case 'Feature':
				return A2(_elm_lang$core$Json_Decode$map, _mgold$elm_geojson$GeoJson$Feature, _mgold$elm_geojson$GeoJson$decodeFeature);
			case 'FeatureCollection':
				return A2(
					_elm_lang$core$Json_Decode$map,
					_mgold$elm_geojson$GeoJson$FeatureCollection,
					A2(
						_elm_lang$core$Json_Decode$field,
						'features',
						_elm_lang$core$Json_Decode$list(_mgold$elm_geojson$GeoJson$decodeFeature)));
			default:
				return A2(_elm_lang$core$Json_Decode$map, _mgold$elm_geojson$GeoJson$Geometry, _mgold$elm_geojson$GeoJson$decodeGeometry);
		}
	};
	return A2(
		_elm_lang$core$Json_Decode$andThen,
		helper,
		A2(_elm_lang$core$Json_Decode$field, 'type', _elm_lang$core$Json_Decode$string));
};
var _mgold$elm_geojson$GeoJson$decoder = A3(
	_elm_lang$core$Json_Decode$map2,
	F2(
		function (v0, v1) {
			return {ctor: '_Tuple2', _0: v0, _1: v1};
		}),
	A2(
		_elm_lang$core$Json_Decode$andThen,
		_mgold$elm_geojson$GeoJson$decodeGeoJson,
		A2(_elm_lang$core$Json_Decode$field, 'type', _elm_lang$core$Json_Decode$string)),
	_elm_lang$core$Json_Decode$maybe(
		A2(_elm_lang$core$Json_Decode$field, 'bbox', _mgold$elm_geojson$GeoJson$decodeBbox)));

var _klaftertief$elm_slippy_map$SlippyMap_Layer_GeoJson_Render$points = F2(
	function (_p0, positionList) {
		var _p1 = _p0;
		return A3(
			_elm_lang$core$List$foldl,
			F2(
				function (position, accum) {
					return function (xy) {
						return A2(
							_elm_lang$core$Basics_ops['++'],
							accum,
							A2(_elm_lang$core$Basics_ops['++'], ' ', xy));
					}(
						function (_p2) {
							var _p3 = _p2;
							return A2(
								_elm_lang$core$Basics_ops['++'],
								_elm_lang$core$Basics$toString(_p3.x),
								A2(
									_elm_lang$core$Basics_ops['++'],
									',',
									_elm_lang$core$Basics$toString(_p3.y)));
						}(
							_p1._0.project(position)));
				}),
			'',
			positionList);
	});
var _klaftertief$elm_slippy_map$SlippyMap_Layer_GeoJson_Render$pathPoints = F2(
	function (_p4, positionList) {
		var _p5 = _p4;
		return function (ll) {
			return A2(_elm_lang$core$Basics_ops['++'], 'M', ll);
		}(
			A2(
				_elm_lang$core$String$join,
				'L',
				A2(
					_elm_lang$core$List$map,
					function (position) {
						return function (_p6) {
							var _p7 = _p6;
							return A2(
								_elm_lang$core$Basics_ops['++'],
								_elm_lang$core$Basics$toString(_p7.x),
								A2(
									_elm_lang$core$Basics_ops['++'],
									' ',
									_elm_lang$core$Basics$toString(_p7.y)));
						}(
							_p5._0.project(position));
					},
					positionList)));
	});
var _klaftertief$elm_slippy_map$SlippyMap_Layer_GeoJson_Render$renderGeoJsonPolygon = F3(
	function (config, attributes, positionListList) {
		var pathDefinition = A2(
			_elm_lang$core$Basics_ops['++'],
			A2(
				_elm_lang$core$String$join,
				' ',
				A2(
					_elm_lang$core$List$map,
					_klaftertief$elm_slippy_map$SlippyMap_Layer_GeoJson_Render$pathPoints(config),
					positionListList)),
			'Z');
		return {
			ctor: '::',
			_0: A2(
				_elm_lang$svg$Svg$path,
				A2(
					_elm_lang$core$Basics_ops['++'],
					attributes,
					{
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$fillRule('evenodd'),
						_1: {
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$d(pathDefinition),
							_1: {ctor: '[]'}
						}
					}),
				{ctor: '[]'}),
			_1: {ctor: '[]'}
		};
	});
var _klaftertief$elm_slippy_map$SlippyMap_Layer_GeoJson_Render$renderGeoJsonLineString = F3(
	function (config, attributes, positionList) {
		return {
			ctor: '::',
			_0: A2(
				_elm_lang$svg$Svg$path,
				A2(
					_elm_lang$core$Basics_ops['++'],
					attributes,
					{
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$points(
							A2(_klaftertief$elm_slippy_map$SlippyMap_Layer_GeoJson_Render$pathPoints, config, positionList)),
						_1: {ctor: '[]'}
					}),
				{ctor: '[]'}),
			_1: {ctor: '[]'}
		};
	});
var _klaftertief$elm_slippy_map$SlippyMap_Layer_GeoJson_Render$renderGeoJsonPoint = F3(
	function (_p8, attributes, position) {
		var _p9 = _p8;
		var _p10 = _p9._0.project(position);
		var x = _p10.x;
		var y = _p10.y;
		return {
			ctor: '::',
			_0: A2(
				_elm_lang$svg$Svg$circle,
				A2(
					_elm_lang$core$Basics_ops['++'],
					attributes,
					{
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$cx(
							_elm_lang$core$Basics$toString(x)),
						_1: {
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$cy(
								_elm_lang$core$Basics$toString(y)),
							_1: {ctor: '[]'}
						}
					}),
				{ctor: '[]'}),
			_1: {ctor: '[]'}
		};
	});
var _klaftertief$elm_slippy_map$SlippyMap_Layer_GeoJson_Render$renderGeoJsonGeometry = F3(
	function (config, attributes, geometry) {
		var _p11 = geometry;
		switch (_p11.ctor) {
			case 'Point':
				return A3(_klaftertief$elm_slippy_map$SlippyMap_Layer_GeoJson_Render$renderGeoJsonPoint, config, attributes, _p11._0);
			case 'MultiPoint':
				return A2(
					_elm_lang$core$List$concatMap,
					A2(_klaftertief$elm_slippy_map$SlippyMap_Layer_GeoJson_Render$renderGeoJsonPoint, config, attributes),
					_p11._0);
			case 'LineString':
				return A3(_klaftertief$elm_slippy_map$SlippyMap_Layer_GeoJson_Render$renderGeoJsonLineString, config, attributes, _p11._0);
			case 'MultiLineString':
				return A2(
					_elm_lang$core$List$concatMap,
					A2(_klaftertief$elm_slippy_map$SlippyMap_Layer_GeoJson_Render$renderGeoJsonLineString, config, attributes),
					_p11._0);
			case 'Polygon':
				return A3(_klaftertief$elm_slippy_map$SlippyMap_Layer_GeoJson_Render$renderGeoJsonPolygon, config, attributes, _p11._0);
			case 'MultiPolygon':
				return A2(
					_elm_lang$core$List$concatMap,
					A2(_klaftertief$elm_slippy_map$SlippyMap_Layer_GeoJson_Render$renderGeoJsonPolygon, config, attributes),
					_p11._0);
			default:
				return A2(
					_elm_lang$core$List$concatMap,
					A2(_klaftertief$elm_slippy_map$SlippyMap_Layer_GeoJson_Render$renderGeoJsonGeometry, config, attributes),
					_p11._0);
		}
	});
var _klaftertief$elm_slippy_map$SlippyMap_Layer_GeoJson_Render$renderGeoJsonFeatureObject = F2(
	function (_p12, featureObject) {
		var _p13 = _p12;
		return A2(
			_elm_lang$core$Maybe$withDefault,
			{ctor: '[]'},
			A2(
				_elm_lang$core$Maybe$map,
				A2(
					_klaftertief$elm_slippy_map$SlippyMap_Layer_GeoJson_Render$renderGeoJsonGeometry,
					_p13,
					_p13._0.style(featureObject)),
				featureObject.geometry));
	});
var _klaftertief$elm_slippy_map$SlippyMap_Layer_GeoJson_Render$renderGeoJsonObject = F2(
	function (config, geoJsonObject) {
		var _p14 = geoJsonObject;
		switch (_p14.ctor) {
			case 'Geometry':
				return A2(
					_klaftertief$elm_slippy_map$SlippyMap_Layer_GeoJson_Render$renderGeoJsonFeatureObject,
					config,
					{
						id: _elm_lang$core$Maybe$Nothing,
						properties: _elm_lang$core$Json_Encode$null,
						geometry: _elm_lang$core$Maybe$Just(_p14._0)
					});
			case 'Feature':
				return A2(_klaftertief$elm_slippy_map$SlippyMap_Layer_GeoJson_Render$renderGeoJsonFeatureObject, config, _p14._0);
			default:
				return A2(
					_elm_lang$core$List$concatMap,
					_klaftertief$elm_slippy_map$SlippyMap_Layer_GeoJson_Render$renderGeoJsonFeatureObject(config),
					_p14._0);
		}
	});
var _klaftertief$elm_slippy_map$SlippyMap_Layer_GeoJson_Render$renderGeoJson = F2(
	function (config, _p15) {
		var _p16 = _p15;
		return A2(
			_elm_lang$svg$Svg$g,
			{ctor: '[]'},
			A2(_klaftertief$elm_slippy_map$SlippyMap_Layer_GeoJson_Render$renderGeoJsonObject, config, _p16._0));
	});
var _klaftertief$elm_slippy_map$SlippyMap_Layer_GeoJson_Render$Config = function (a) {
	return {ctor: 'Config', _0: a};
};

var _klaftertief$elm_slippy_map$SlippyMap_Layer_GeoJson$render = F3(
	function (_p1, geoJson, _p0) {
		var _p2 = _p1;
		var _p3 = _p0;
		var project = function (_p4) {
			var _p5 = _p4;
			return _p3.locationToContainerPoint(
				{lon: _p5._0, lat: _p5._1});
		};
		var renderConfig = _klaftertief$elm_slippy_map$SlippyMap_Layer_GeoJson_Render$Config(
			{project: project, style: _p2._0.style});
		var centerPoint = _p3.centerPoint;
		return A2(
			_elm_lang$svg$Svg$g,
			{ctor: '[]'},
			{
				ctor: '::',
				_0: A2(_klaftertief$elm_slippy_map$SlippyMap_Layer_GeoJson_Render$renderGeoJson, renderConfig, geoJson),
				_1: {ctor: '[]'}
			});
	});
var _klaftertief$elm_slippy_map$SlippyMap_Layer_GeoJson$layer = F2(
	function (config, geoJson) {
		return A2(
			_klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$withRender,
			_klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$overlay,
			A2(_klaftertief$elm_slippy_map$SlippyMap_Layer_GeoJson$render, config, geoJson));
	});
var _klaftertief$elm_slippy_map$SlippyMap_Layer_GeoJson$Config = function (a) {
	return {ctor: 'Config', _0: a};
};
var _klaftertief$elm_slippy_map$SlippyMap_Layer_GeoJson$defaultConfig = _klaftertief$elm_slippy_map$SlippyMap_Layer_GeoJson$Config(
	{
		style: _elm_lang$core$Basics$always(
			{
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$stroke('#3388ff'),
				_1: {
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$strokeWidth('3'),
					_1: {
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$fill('#3388ff'),
						_1: {
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$fillOpacity('0.2'),
							_1: {
								ctor: '::',
								_0: _elm_lang$svg$Svg_Attributes$strokeLinecap('round'),
								_1: {
									ctor: '::',
									_0: _elm_lang$svg$Svg_Attributes$strokeLinejoin('round'),
									_1: {ctor: '[]'}
								}
							}
						}
					}
				}
			})
	});

var _klaftertief$elm_slippy_map$SlippyMap_Layer_Grid$line = F2(
	function (color, _p0) {
		var _p1 = _p0;
		var _p5 = _p1._1;
		var _p4 = _p1._0;
		return A2(
			_elm_lang$svg$Svg$line,
			{
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$x1(
					_elm_lang$core$Basics$toString(_p4.x)),
				_1: {
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$y1(
						_elm_lang$core$Basics$toString(_p4.y)),
					_1: {
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$x2(
							_elm_lang$core$Basics$toString(_p5.x)),
						_1: {
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$y2(
								_elm_lang$core$Basics$toString(_p5.y)),
							_1: {
								ctor: '::',
								_0: _elm_lang$svg$Svg_Attributes$stroke(
									function (_p2) {
										var _p3 = _p2;
										return A2(
											_elm_lang$core$Basics_ops['++'],
											'rgba(',
											A2(
												_elm_lang$core$Basics_ops['++'],
												_elm_lang$core$Basics$toString(_p3.red),
												A2(
													_elm_lang$core$Basics_ops['++'],
													',',
													A2(
														_elm_lang$core$Basics_ops['++'],
														_elm_lang$core$Basics$toString(_p3.green),
														A2(
															_elm_lang$core$Basics_ops['++'],
															',',
															A2(
																_elm_lang$core$Basics_ops['++'],
																_elm_lang$core$Basics$toString(_p3.blue),
																A2(
																	_elm_lang$core$Basics_ops['++'],
																	',',
																	A2(
																		_elm_lang$core$Basics_ops['++'],
																		_elm_lang$core$Basics$toString(_p3.alpha),
																		')'))))))));
									}(
										_elm_lang$core$Color$toRgb(color))),
								_1: {
									ctor: '::',
									_0: _elm_lang$svg$Svg_Attributes$strokeWidth('1'),
									_1: {
										ctor: '::',
										_0: _elm_lang$svg$Svg_Attributes$shapeRendering('crispEdges'),
										_1: {ctor: '[]'}
									}
								}
							}
						}
					}
				}
			},
			{ctor: '[]'});
	});
var _klaftertief$elm_slippy_map$SlippyMap_Layer_Grid$render = F2(
	function (_p6, renderState) {
		var _p7 = _p6;
		var _p9 = _p7._0;
		var _p8 = renderState.locationBounds;
		var southWest = _p8.southWest;
		var northEast = _p8.northEast;
		var lons = A2(
			_elm_lang$core$List$map,
			function (lon) {
				return {
					ctor: '_Tuple2',
					_0: renderState.locationToContainerPoint(
						{lon: lon, lat: southWest.lat}),
					_1: renderState.locationToContainerPoint(
						{lon: lon, lat: northEast.lat})
				};
			},
			A2(
				_elm_lang$core$List$map,
				_elm_lang$core$Basics$toFloat,
				A2(
					_elm_lang$core$List$range,
					_elm_lang$core$Basics$floor(southWest.lon),
					_elm_lang$core$Basics$ceiling(northEast.lon))));
		var lats = A2(
			_elm_lang$core$List$map,
			function (lat) {
				return {
					ctor: '_Tuple2',
					_0: renderState.locationToContainerPoint(
						{lon: southWest.lon, lat: lat}),
					_1: renderState.locationToContainerPoint(
						{lon: northEast.lon, lat: lat})
				};
			},
			A2(
				_elm_lang$core$List$map,
				_elm_lang$core$Basics$toFloat,
				A2(
					_elm_lang$core$List$range,
					_elm_lang$core$Basics$floor(southWest.lat),
					_elm_lang$core$Basics$ceiling(northEast.lat))));
		return A2(
			_elm_lang$svg$Svg$g,
			{ctor: '[]'},
			A2(
				_elm_lang$core$Basics_ops['++'],
				A2(
					_elm_lang$core$List$map,
					_klaftertief$elm_slippy_map$SlippyMap_Layer_Grid$line(_p9.majorTickColor),
					lons),
				A2(
					_elm_lang$core$List$map,
					_klaftertief$elm_slippy_map$SlippyMap_Layer_Grid$line(_p9.majorTickColor),
					lats)));
	});
var _klaftertief$elm_slippy_map$SlippyMap_Layer_Grid$layer = function (config) {
	return A2(
		_klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$withRender,
		_klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$overlay,
		_klaftertief$elm_slippy_map$SlippyMap_Layer_Grid$render(config));
};
var _klaftertief$elm_slippy_map$SlippyMap_Layer_Grid$Config = function (a) {
	return {ctor: 'Config', _0: a};
};
var _klaftertief$elm_slippy_map$SlippyMap_Layer_Grid$defaultConfig = _klaftertief$elm_slippy_map$SlippyMap_Layer_Grid$Config(
	{majorTickColor: _elm_lang$core$Color$black, minorTickColor: _elm_lang$core$Color$grey});

var _klaftertief$elm_slippy_map$SlippyMap_Layer_Heatmap$render = F3(
	function (_p0, dataLocations, renderState) {
		var _p1 = _p0;
		var bounds = renderState.locationBounds;
		var dataLocationsFiltered = A2(
			_elm_lang$core$List$filter,
			function (_p2) {
				var _p3 = _p2;
				return A2(_klaftertief$elm_slippy_map$SlippyMap_Geo_Location$isInsideBounds, bounds, _p3._0);
			},
			dataLocations);
		return A2(
			_elm_lang$svg$Svg$g,
			{ctor: '[]'},
			{
				ctor: '::',
				_0: A2(
					_klaftertief$elm_heatmap$Heatmap$view,
					_p1._0.heatmap(renderState),
					dataLocationsFiltered),
				_1: {ctor: '[]'}
			});
	});
var _klaftertief$elm_slippy_map$SlippyMap_Layer_Heatmap$layer = F2(
	function (config, dataLocations) {
		return A2(
			_klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$withRender,
			_klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$overlay,
			A2(_klaftertief$elm_slippy_map$SlippyMap_Layer_Heatmap$render, config, dataLocations));
	});
var _klaftertief$elm_slippy_map$SlippyMap_Layer_Heatmap$defaultGradient = {
	ctor: '::',
	_0: {
		ctor: '_Tuple2',
		_0: 0.4,
		_1: A3(_elm_lang$core$Color$rgb, 0, 0, 255)
	},
	_1: {
		ctor: '::',
		_0: {
			ctor: '_Tuple2',
			_0: 0.5,
			_1: A3(_elm_lang$core$Color$rgb, 0, 255, 255)
		},
		_1: {
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: 0.7,
				_1: A3(_elm_lang$core$Color$rgb, 0, 255, 0)
			},
			_1: {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 0.8,
					_1: A3(_elm_lang$core$Color$rgb, 255, 255, 0)
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 1,
						_1: A3(_elm_lang$core$Color$rgb, 255, 0, 0)
					},
					_1: {ctor: '[]'}
				}
			}
		}
	}
};
var _klaftertief$elm_slippy_map$SlippyMap_Layer_Heatmap$defaultHeatmapConfig = function (renderState) {
	return A2(
		_klaftertief$elm_heatmap$Heatmap$withRadius,
		renderState.zoom * 2,
		_klaftertief$elm_heatmap$Heatmap$config(
			{
				toPoint: function (_p4) {
					var _p5 = _p4;
					var _p6 = renderState.locationToContainerPoint(_p5._0);
					var x = _p6.x;
					var y = _p6.y;
					return {x: x, y: y, weight: _p5._1};
				},
				gradient: _klaftertief$elm_slippy_map$SlippyMap_Layer_Heatmap$defaultGradient
			}));
};
var _klaftertief$elm_slippy_map$SlippyMap_Layer_Heatmap$Config = function (a) {
	return {ctor: 'Config', _0: a};
};
var _klaftertief$elm_slippy_map$SlippyMap_Layer_Heatmap$defaultConfig = _klaftertief$elm_slippy_map$SlippyMap_Layer_Heatmap$Config(
	{heatmap: _klaftertief$elm_slippy_map$SlippyMap_Layer_Heatmap$defaultHeatmapConfig});

var _klaftertief$elm_slippy_map$SlippyMap_Layer_Tile$tile = F3(
	function (render, renderState, _p0) {
		var _p1 = _p0;
		var _p4 = _p1.z;
		var _p3 = _p1.y;
		var _p2 = _p1.x;
		var tileCoordinate = {
			column: _elm_lang$core$Basics$toFloat(_p2),
			row: _elm_lang$core$Basics$toFloat(_p3),
			zoom: _elm_lang$core$Basics$toFloat(_p4)
		};
		var point = renderState.coordinateToContainerPoint(tileCoordinate);
		var key = A2(
			_elm_lang$core$Basics_ops['++'],
			_elm_lang$core$Basics$toString(_p4),
			A2(
				_elm_lang$core$Basics_ops['++'],
				'/',
				A2(
					_elm_lang$core$Basics_ops['++'],
					_elm_lang$core$Basics$toString(
						A2(
							_elm_lang$core$Basics_ops['%'],
							_p2,
							Math.pow(2, _p4))),
					A2(
						_elm_lang$core$Basics_ops['++'],
						'/',
						_elm_lang$core$Basics$toString(
							A2(
								_elm_lang$core$Basics_ops['%'],
								_p3,
								Math.pow(2, _p4)))))));
		return {
			ctor: '_Tuple2',
			_0: key,
			_1: A2(
				_elm_lang$svg$Svg$g,
				{
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$class('tile'),
					_1: {
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$transform(
							A2(
								_elm_lang$core$Basics_ops['++'],
								'translate(',
								A2(
									_elm_lang$core$Basics_ops['++'],
									_elm_lang$core$Basics$toString(point.x),
									A2(
										_elm_lang$core$Basics_ops['++'],
										' ',
										A2(
											_elm_lang$core$Basics_ops['++'],
											_elm_lang$core$Basics$toString(point.y),
											')'))))),
						_1: {ctor: '[]'}
					}
				},
				{
					ctor: '::',
					_0: render(_p1),
					_1: {ctor: '[]'}
				})
		};
	});
var _klaftertief$elm_slippy_map$SlippyMap_Layer_Tile$render = F3(
	function (fromTile, renderTile, renderState) {
		var tiles = renderState.tileCover;
		var centerPoint = renderState.halfSize;
		var scale = renderState.tileScale;
		return A3(
			_elm_lang$svg$Svg_Keyed$node,
			'g',
			{ctor: '[]'},
			A2(
				_elm_lang$core$List$map,
				A2(
					_klaftertief$elm_slippy_map$SlippyMap_Layer_Tile$tile,
					function (_p5) {
						return A2(
							renderTile,
							renderState,
							fromTile(_p5));
					},
					renderState),
				tiles));
	});
var _klaftertief$elm_slippy_map$SlippyMap_Layer_Tile$layer = F3(
	function (fromTile, renderTile, _p6) {
		var _p7 = _p6;
		return A2(
			_klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$withRender,
			_p7._0.layerConfig,
			A2(_klaftertief$elm_slippy_map$SlippyMap_Layer_Tile$render, fromTile, renderTile));
	});
var _klaftertief$elm_slippy_map$SlippyMap_Layer_Tile$ConfigInternal = function (a) {
	return {layerConfig: a};
};
var _klaftertief$elm_slippy_map$SlippyMap_Layer_Tile$Config = function (a) {
	return {ctor: 'Config', _0: a};
};
var _klaftertief$elm_slippy_map$SlippyMap_Layer_Tile$config = _klaftertief$elm_slippy_map$SlippyMap_Layer_Tile$Config(
	{layerConfig: _klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$tile});
var _klaftertief$elm_slippy_map$SlippyMap_Layer_Tile$withAttribution = F2(
	function (attribution, _p8) {
		var _p9 = _p8;
		var _p10 = _p9._0;
		return _klaftertief$elm_slippy_map$SlippyMap_Layer_Tile$Config(
			_elm_lang$core$Native_Utils.update(
				_p10,
				{
					layerConfig: A2(_klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$withAttribution, attribution, _p10.layerConfig)
				}));
	});

var _klaftertief$elm_slippy_map$SlippyMap_Layer_Marker$renderMarker = F3(
	function (_p2, _p1, _p0) {
		var _p3 = _p2;
		var _p4 = _p1;
		var _p5 = _p0;
		var markerPoint = _p4.locationToContainerPoint(_p5._0);
		return A2(
			_elm_lang$svg$Svg$g,
			{
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$transform(
					A2(
						_elm_lang$core$Basics_ops['++'],
						'translate(',
						A2(
							_elm_lang$core$Basics_ops['++'],
							_elm_lang$core$Basics$toString(markerPoint.x),
							A2(
								_elm_lang$core$Basics_ops['++'],
								' ',
								A2(
									_elm_lang$core$Basics_ops['++'],
									_elm_lang$core$Basics$toString(markerPoint.y),
									')'))))),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: _p3._0.renderMarker(_p5._1),
				_1: {ctor: '[]'}
			});
	});
var _klaftertief$elm_slippy_map$SlippyMap_Layer_Marker$render = F3(
	function (config, locatedMarkers, _p6) {
		var _p7 = _p6;
		var _p10 = _p7;
		var bounds = _p10.locationBounds;
		var locatedMarkersFiltered = A2(
			_elm_lang$core$List$filter,
			function (_p8) {
				var _p9 = _p8;
				return A2(_klaftertief$elm_slippy_map$SlippyMap_Geo_Location$isInsideBounds, bounds, _p9._0);
			},
			locatedMarkers);
		var centerPoint = _p10.centerPoint;
		return A2(
			_elm_lang$svg$Svg$g,
			{ctor: '[]'},
			A2(
				_elm_lang$core$List$map,
				A2(_klaftertief$elm_slippy_map$SlippyMap_Layer_Marker$renderMarker, config, _p10),
				locatedMarkersFiltered));
	});
var _klaftertief$elm_slippy_map$SlippyMap_Layer_Marker$layer = F2(
	function (config, locatedMarkers) {
		return A2(
			_klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$withRender,
			_klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$marker,
			A2(_klaftertief$elm_slippy_map$SlippyMap_Layer_Marker$render, config, locatedMarkers));
	});
var _klaftertief$elm_slippy_map$SlippyMap_Layer_Marker$simpleLayer = F2(
	function (config, locations) {
		var locatedMarkers = A2(
			_elm_lang$core$List$map,
			function (location) {
				return {
					ctor: '_Tuple2',
					_0: location,
					_1: {ctor: '_Tuple0'}
				};
			},
			locations);
		return A2(
			_klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$withRender,
			_klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$marker,
			A2(_klaftertief$elm_slippy_map$SlippyMap_Layer_Marker$render, config, locatedMarkers));
	});
var _klaftertief$elm_slippy_map$SlippyMap_Layer_Marker$circleMarker = A2(
	_elm_lang$svg$Svg$circle,
	{
		ctor: '::',
		_0: _elm_lang$svg$Svg_Attributes$r('8'),
		_1: {
			ctor: '::',
			_0: _elm_lang$svg$Svg_Attributes$fill('#3388ff'),
			_1: {
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$stroke('white'),
				_1: {
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$strokeWidth('3'),
					_1: {ctor: '[]'}
				}
			}
		}
	},
	{ctor: '[]'});
var _klaftertief$elm_slippy_map$SlippyMap_Layer_Marker$Config = function (a) {
	return {ctor: 'Config', _0: a};
};
var _klaftertief$elm_slippy_map$SlippyMap_Layer_Marker$defaultConfig = _klaftertief$elm_slippy_map$SlippyMap_Layer_Marker$Config(
	{
		renderMarker: _elm_lang$core$Basics$always(_klaftertief$elm_slippy_map$SlippyMap_Layer_Marker$circleMarker)
	});

var _klaftertief$elm_slippy_map$SlippyMap_Layer_Overlay$renderOverlay = F3(
	function (_p2, _p1, _p0) {
		var _p3 = _p2;
		var _p4 = _p1;
		var _p7 = _p4.locationToContainerPoint;
		var _p5 = _p0;
		var _p6 = _p5._0;
		var northEastPoint = _p7(_p6.northEast);
		var southWestPoint = _p7(_p6.southWest);
		var overlaySize = {ctor: '_Tuple2', _0: northEastPoint.x - southWestPoint.x, _1: southWestPoint.y - northEastPoint.y};
		return A2(
			_elm_lang$svg$Svg$g,
			{
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$transform(
					A2(
						_elm_lang$core$Basics_ops['++'],
						'translate(',
						A2(
							_elm_lang$core$Basics_ops['++'],
							_elm_lang$core$Basics$toString(southWestPoint.x),
							A2(
								_elm_lang$core$Basics_ops['++'],
								' ',
								A2(
									_elm_lang$core$Basics_ops['++'],
									_elm_lang$core$Basics$toString(northEastPoint.y),
									')'))))),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A2(_p3._0.renderOverlay, overlaySize, _p5._1),
				_1: {ctor: '[]'}
			});
	});
var _klaftertief$elm_slippy_map$SlippyMap_Layer_Overlay$render = F3(
	function (config, boundedOverlays, renderState) {
		return A2(
			_elm_lang$svg$Svg$g,
			{ctor: '[]'},
			A2(
				_elm_lang$core$List$map,
				A2(_klaftertief$elm_slippy_map$SlippyMap_Layer_Overlay$renderOverlay, config, renderState),
				boundedOverlays));
	});
var _klaftertief$elm_slippy_map$SlippyMap_Layer_Overlay$layer = F2(
	function (config, boundedOverlays) {
		return A2(
			_klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$withRender,
			_klaftertief$elm_slippy_map$SlippyMap_Layer_LowLevel$overlay,
			A2(_klaftertief$elm_slippy_map$SlippyMap_Layer_Overlay$render, config, boundedOverlays));
	});
var _klaftertief$elm_slippy_map$SlippyMap_Layer_Overlay$imageOverlay = F2(
	function (_p8, url) {
		var _p9 = _p8;
		return A2(
			_elm_lang$svg$Svg$image,
			{
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$width(
					_elm_lang$core$Basics$toString(_p9._0)),
				_1: {
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$height(
						_elm_lang$core$Basics$toString(_p9._1)),
					_1: {
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$xlinkHref(url),
						_1: {ctor: '[]'}
					}
				}
			},
			{ctor: '[]'});
	});
var _klaftertief$elm_slippy_map$SlippyMap_Layer_Overlay$Config = function (a) {
	return {ctor: 'Config', _0: a};
};
var _klaftertief$elm_slippy_map$SlippyMap_Layer_Overlay$defaultConfig = _klaftertief$elm_slippy_map$SlippyMap_Layer_Overlay$Config(
	{renderOverlay: _klaftertief$elm_slippy_map$SlippyMap_Layer_Overlay$imageOverlay});

var _klaftertief$elm_slippy_map$SlippyMap_Layer_StaticImage$replace = F3(
	function (search, substitution, string) {
		return A4(
			_elm_lang$core$Regex$replace,
			_elm_lang$core$Regex$All,
			_elm_lang$core$Regex$regex(
				_elm_lang$core$Regex$escape(search)),
			function (_p0) {
				return substitution;
			},
			string);
	});
var _klaftertief$elm_slippy_map$SlippyMap_Layer_StaticImage$tile = F3(
	function (_p2, renderState, _p1) {
		var _p3 = _p2;
		var _p4 = _p1;
		return A2(
			_elm_lang$svg$Svg$image,
			{
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$width(
					_elm_lang$core$Basics$toString(renderState.transform.tileSize)),
				_1: {
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$height(
						_elm_lang$core$Basics$toString(renderState.transform.tileSize)),
					_1: {
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$xlinkHref(
							_p3._1.toUrl(_p4)),
						_1: {
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$transform(
								A2(
									_elm_lang$core$Basics_ops['++'],
									'scale(',
									A2(
										_elm_lang$core$Basics_ops['++'],
										_elm_lang$core$Basics$toString(renderState.tileScale),
										')'))),
							_1: {ctor: '[]'}
						}
					}
				}
			},
			{ctor: '[]'});
	});
var _klaftertief$elm_slippy_map$SlippyMap_Layer_StaticImage$layer = function (_p5) {
	var _p6 = _p5;
	return A3(
		_klaftertief$elm_slippy_map$SlippyMap_Layer_Tile$layer,
		_elm_lang$core$Basics$identity,
		_klaftertief$elm_slippy_map$SlippyMap_Layer_StaticImage$tile(_p6),
		_p6._0);
};
var _klaftertief$elm_slippy_map$SlippyMap_Layer_StaticImage$ConfigInternal = function (a) {
	return {toUrl: a};
};
var _klaftertief$elm_slippy_map$SlippyMap_Layer_StaticImage$Config = F2(
	function (a, b) {
		return {ctor: 'Config', _0: a, _1: b};
	});
var _klaftertief$elm_slippy_map$SlippyMap_Layer_StaticImage$config = F2(
	function (urlTemplate, subDomains) {
		var toUrl = function (_p7) {
			var _p8 = _p7;
			var _p11 = _p8.z;
			var _p10 = _p8.y;
			var _p9 = _p8.x;
			return A3(
				_klaftertief$elm_slippy_map$SlippyMap_Layer_StaticImage$replace,
				'{s}',
				A2(
					_elm_lang$core$Maybe$withDefault,
					'',
					_elm_lang$core$List$head(
						A3(
							_elm_lang$core$Basics$flip,
							_elm_lang$core$List$drop,
							subDomains,
							A2(
								_elm_lang$core$Basics_ops['%'],
								_elm_lang$core$Basics$abs(_p9 + _p10),
								A2(
									_elm_lang$core$Basics$max,
									1,
									_elm_lang$core$List$length(subDomains)))))),
				A3(
					_klaftertief$elm_slippy_map$SlippyMap_Layer_StaticImage$replace,
					'{y}',
					_elm_lang$core$Basics$toString(
						A2(
							_elm_lang$core$Basics_ops['%'],
							_p10,
							Math.pow(2, _p11))),
					A3(
						_klaftertief$elm_slippy_map$SlippyMap_Layer_StaticImage$replace,
						'{x}',
						_elm_lang$core$Basics$toString(
							A2(
								_elm_lang$core$Basics_ops['%'],
								_p9,
								Math.pow(2, _p11))),
						A3(
							_klaftertief$elm_slippy_map$SlippyMap_Layer_StaticImage$replace,
							'{z}',
							_elm_lang$core$Basics$toString(
								A2(_elm_lang$core$Basics$max, 0, _p11)),
							urlTemplate))));
		};
		return A2(
			_klaftertief$elm_slippy_map$SlippyMap_Layer_StaticImage$Config,
			_klaftertief$elm_slippy_map$SlippyMap_Layer_Tile$config,
			{toUrl: toUrl});
	});
var _klaftertief$elm_slippy_map$SlippyMap_Layer_StaticImage$withAttribution = F2(
	function (attribution, _p12) {
		var _p13 = _p12;
		return A2(
			_klaftertief$elm_slippy_map$SlippyMap_Layer_StaticImage$Config,
			A2(_klaftertief$elm_slippy_map$SlippyMap_Layer_Tile$withAttribution, attribution, _p13._0),
			_p13._1);
	});

var _user$project$Data$someLocationDataJson = '\n[\n[-37.8839, 175.3745188667, \"571\"],\n[-37.8869090667, 175.3657417333, \"486\"],\n[-37.8894207167, 175.4015351167, \"807\"],\n[-37.8927369333, 175.4087452333, \"899\"],\n[-37.90585105, 175.4453463833, \"1273\"],\n[-37.9064188833, 175.4441556833, \"1258\"],\n[-37.90584715, 175.4463564333, \"1279\"],\n[-37.9033391333, 175.4244005667, \"1078\"],\n[-37.9061991333, 175.4492620333, \"1309\"],\n[-37.9058955167, 175.4445613167, \"1261\"],\n[-37.88888045, 175.39146475, \"734\"],\n[-37.8950811333, 175.41079175, \"928\"],\n[-37.88909235, 175.3922956333, \"740\"],\n[-37.8889259667, 175.3938591667, \"759\"],\n[-37.8876576333, 175.3859563833, \"687\"],\n[-37.89027155, 175.3973178833, \"778\"],\n[-37.8864473667, 175.3806136833, \"631\"],\n[-37.9000262833, 175.4183242167, \"1012\"],\n[-37.90036495, 175.4189457, \"1024\"],\n[-37.9000976833, 175.4197312167, \"1027\"],\n[-37.90239975, 175.42371165, \"1067\"],\n[-37.9043379667, 175.42430325, \"1080\"],\n[-37.9026441, 175.4231055167, \"1068\"],\n[-37.8883536333, 175.3888573833, \"718\"],\n[-37.9029948833, 175.4237386167, \"1070\"],\n[-37.89824135, 175.4150421667, \"982\"],\n[-37.8976067833, 175.41510265, \"983\"],\n[-37.9023491333, 175.4225495, \"1066\"],\n[-37.8856157167, 175.3775632833, \"608\"],\n[-37.8963032667, 175.4132068, \"951\"],\n[-37.8922813667, 175.4073402333, \"1/898\"],\n[-37.88933345, 175.3956084333, \"769\"],\n[-37.8936148833, 175.4090577, \"906\"],\n[-37.8939398, 175.4094444833, \"914\"],\n[-37.8857355333, 175.3722297667, \"542\"],\n[-37.8931092167, 175.4083014, \"898\"],\n[-37.9008253167, 175.4198128, \"1030\"],\n[-37.9045052333, 175.4260735, \"1100\"],\n[-37.9053927167, 175.42822265, \"1130\"],\n[-37.90507935, 175.4313065, \"1147\"],\n[-37.9055749667, 175.4319092167, \"1154\"],\n[-37.9039034833, 175.4274736667, \"1/1105\"],\n[-37.9037633, 175.4261181833, \"1093\"],\n[-37.9038755, 175.42871045, \"3/1105\"],\n[-37.90369555, 175.4285285, \"2/1105\"],\n[-37.9056626, 175.4341078833, \"1170\"],\n[-37.9018736833, 175.438852, \"1/1213\"],\n[-37.9057596167, 175.4356650167, \"1180\"],\n[-37.9053502, 175.4361049333, \"1185\"],\n[-37.9053379167, 175.4366986167, \"1195\"],\n[-37.9058892333, 175.4381450333, \"1204\"],\n[-37.9060264167, 175.4400763167, \"1220\"],\n[-37.9056766833, 175.4412592, \"1233\"],\n[-37.9057312167, 175.4418380333, \"1235\"],\n[-37.9061575833, 175.4421068667, \"1242\"],\n[-37.9063946167, 175.4438004667, \"1256\"],\n[-37.8996027667, 175.43995055, \"4/1215\"],\n[-37.9006449667, 175.4395556833, \"3/1215\"],\n[-37.9009138167, 175.4394061333, \"2/1215\"],\n[-37.9034547, 175.4396315, \"1219\"],\n[-37.9055243, 175.4396033, \"1221\"],\n[-37.89952325, 175.4406619167, \"5/1215\"],\n[-37.90561525, 175.4404853167, \"1225\"],\n[-37.9045602333, 175.4477690333, \"1285\"],\n[-37.9040051667, 175.4388491833, \"1213\"],\n[-37.90588145, 175.4440349167, \"1257\"],\n[-37.90595915, 175.4389286833, \"1212\"],\n[-37.9059939667, 175.4398068833, \"1218\"],\n[-37.8868631833, 175.37991055, \"630\"],\n[-37.8878744833, 175.382179, \"650\"],\n[-37.8880764, 175.3839845667, \"670\"],\n[-37.8850457333, 175.3759821, \"594\"],\n[-37.88446045, 175.3762872667, \"587\"],\n[-37.8880782667, 175.38423415, \"1/670\"],\n[-37.8863533833, 175.3690698667, \"515\"],\n[-37.8861783167, 175.3710009833, \"530\"],\n[-37.885424, 175.3716677833, \"541\"],\n[-37.88524065, 175.3722141167, \"547\"],\n[-37.9022371333, 175.47991035, \"10\"],\n[-37.9020014833, 175.4799581667, \"1\"],\n[-37.9020824, 175.4802630167, \"2\"],\n[-37.9018589833, 175.4804760833, \"3\"],\n[-37.9018211333, 175.4806769667, \"4\"],\n[-37.9021543667, 175.4805538833, \"5\"],\n[-37.9022658, 175.4807579333, \"6\"],\n[-37.9024517833, 175.4806480667, \"7\"],\n[-37.9024251167, 175.48041985, \"8\"],\n[-37.9023317833, 175.4802119667, \"9\"],\n[-37.9321212167, 175.4555088, \"39\"],\n[-37.8956185167, 175.4719458667, \"4\"],\n[-37.8954566, 175.4728120333, \"20\"],\n[-37.8957231833, 175.4727906, \"22A\"],\n[-37.8956085833, 175.4726702, \"22\"],\n[-37.8956460167, 175.4718485167, \"2\"],\n[-37.8953487167, 175.47202915, \"5\"],\n[-37.8800121167, 175.4865467167, \"9\"],\n[-37.8803487833, 175.48595255, \"3\"],\n[-37.8802064167, 175.4861004, \"5\"],\n[-37.8800705167, 175.4862671167, \"7\"],\n[-37.8798887333, 175.4863712333, \"7A\"],\n[-37.8801676667, 175.4866722667, \"10\"],\n[-37.88029245, 175.4868499667, \"8\"],\n[-37.8803302167, 175.4865822167, \"6\"],\n[-37.88038715, 175.4864004167, \"4\"],\n[-37.8805029333, 175.4862314167, \"2\"],\n[-37.9127148667, 175.4710607833, \"51\"],\n[-37.9118609667, 175.4668648, \"20\"],\n[-37.9122010667, 175.47078695, \"49A\"],\n[-37.91191245, 175.4682913833, \"29\"],\n[-37.9112774333, 175.4668027333, \"17A\"],\n[-37.91244995, 175.4700709833, \"41\"],\n[-37.9149636, 175.4772568333, \"98\"],\n[-37.9128421833, 175.4702103167, \"42\"],\n[-37.91130515, 175.4650217667, \"2\"],\n[-37.9140405333, 175.4754503833, \"85\"],\n[-37.91155815, 175.4670938833, \"21\"],\n[-37.9144416167, 175.4754564, \"86\"],\n[-37.91149715, 175.4668828667, \"19\"],\n[-37.9155068167, 175.4784839167, \"116\"],\n[-37.9135311667, 175.4736794833, \"69\"],\n[-37.9146717667, 175.4773664833, \"103\"],\n[-37.9135175667, 175.4724437333, \"62\"],\n[-37.9117463, 175.4676612167, \"23\"],\n[-37.9136108833, 175.47263915, \"64\"],\n[-37.9118005167, 175.46788515, \"25\"],\n[-37.9142630167, 175.4748833333, \"80\"],\n[-37.9118481833, 175.4680930167, \"27\"],\n[-37.91519165, 175.47727755, \"100\"],\n[-37.9121701, 175.4679073167, \"28\"],\n[-37.9152358167, 175.4780924833, \"112\"],\n[-37.9122425667, 175.4681859167, \"30\"],\n[-37.9150027167, 175.47843285, \"107\"],\n[-37.91196865, 175.4684916833, \"31\"],\n[-37.9132330333, 175.4726685333, \"61\"],\n[-37.9123722, 175.4685087667, \"32\"],\n[-37.9151754667, 175.4790262, \"113\"],\n[-37.9120319833, 175.46868985, \"33\"],\n[-37.9151328167, 175.4788729, \"111\"],\n[-37.9124617167, 175.4687799833, \"34\"],\n[-37.9150617167, 175.4786454167, \"109\"],\n[-37.9120926, 175.4688931667, \"35\"],\n[-37.9132881333, 175.47285965, \"63\"],\n[-37.9119984333, 175.4691844, \"37A\"],\n[-37.9120311, 175.4673706667, \"24\"],\n[-37.91214925, 175.46909885, \"37\"],\n[-37.91408025, 175.4759690833, \"91B\"],\n[-37.9125366, 175.4691343, \"38\"],\n[-37.9134794833, 175.4739836167, \"71A\"],\n[-37.9122081167, 175.4674649333, \"26A\"],\n[-37.9140814333, 175.4736708667, \"72A\"],\n[-37.9120801, 175.4675947333, \"26\"],\n[-37.9113324167, 175.46512405, \"4\"],\n[-37.91185795, 175.4686138167, \"31A\"],\n[-37.9144403167, 175.4767387667, \"101\"],\n[-37.9125054167, 175.46896025, \"36A\"],\n[-37.9151334833, 175.4778022667, \"106\"],\n[-37.9126167833, 175.4688409667, \"36B\"],\n[-37.9111576, 175.4663765167, \"13A\"],\n[-37.9112960833, 175.4662379, \"13\"],\n[-37.9116252167, 175.46602135, \"14\"],\n[-37.9113666167, 175.4664507833, \"15\"],\n[-37.9117068333, 175.466336, \"16\"],\n[-37.9114338333, 175.4666576, \"17\"],\n[-37.9119338667, 175.4665694167, \"18A\"],\n[-37.9117808333, 175.4665752, \"18\"],\n[-37.9110205, 175.4652438667, \"3\"],\n[-37.9110742833, 175.4654501167, \"5\"],\n[-37.9111370833, 175.4656566833, \"7\"],\n[-37.9111865833, 175.4658542667, \"9\"],\n[-37.9112390333, 175.46602075, \"11\"],\n[-37.9118135167, 175.46543705, \"6A\"],\n[-37.9118572167, 175.46556135, \"6B\"],\n[-37.91145615, 175.4655286, \"6\"],\n[-37.9115389167, 175.4657957167, \"8\"],\n[-37.9127748333, 175.4699760667, \"40\"],\n[-37.9125127167, 175.4703133, \"43\"],\n[-37.9129274, 175.4704172833, \"44\"],\n[-37.9125759833, 175.4705303667, \"45\"],\n[-37.9129758667, 175.4706118, \"46\"],\n[-37.9126359667, 175.4707644, \"47\"],\n[-37.91226225, 175.47106665, \"49\"],\n[-37.9130937833, 175.4709588833, \"50\"],\n[-37.9131644667, 175.4711523, \"52\"],\n[-37.9132299667, 175.4713462167, \"60\"],\n[-37.9127690833, 175.4712279667, \"53\"],\n[-37.9133607167, 175.4730695833, \"65\"],\n[-37.91367805, 175.4728816667, \"66\"],\n[-37.9134211, 175.4732760667, \"67\"],\n[-37.9137477833, 175.4731176, \"68\"],\n[-37.9138932333, 175.4736511667, \"70\"],\n[-37.9135950667, 175.4738879833, \"71\"],\n[-37.9139430167, 175.4737982333, \"72\"],\n[-37.9136486, 175.4740868667, \"73\"],\n[-37.91400415, 175.4740125833, \"74\"],\n[-37.9140350333, 175.4741693833, \"76\"],\n[-37.91432385, 175.475081, \"82\"],\n[-37.9139975333, 175.47523055, \"83\"],\n[-37.9143889667, 175.47526065, \"84\"],\n[-37.9137640333, 175.47575135, \"87\"],\n[-37.91449875, 175.4756521167, \"88\"],\n[-37.9141123, 175.4756848833, \"89\"],\n[-37.9145492167, 175.4758458667, \"90\"],\n[-37.9141779667, 175.4758650667, \"91\"],\n[-37.9146104833, 175.4760345, \"92\"],\n[-37.9142320333, 175.4760478833, \"93\"],\n[-37.9146642167, 175.47621125, \"94\"],\n[-37.9142896667, 175.4762277667, \"95\"],\n[-37.9147136833, 175.4764402833, \"96\"],\n[-37.9143434167, 175.47639805, \"97\"],\n[-37.9143937167, 175.4765685, \"99\"],\n[-37.91501315, 175.4774403667, \"102\"],\n[-37.9154860667, 175.4774428167, \"104\"],\n[-37.9149432667, 175.4782801, \"105\"],\n[-37.9152651667, 175.47833035, \"114\"],\n[-37.9299333167, 175.55909085, \"128\"],\n[-37.9286782833, 175.5545978, \"181\"],\n[-37.9300747333, 175.5497311333, \"3/193\"],\n[-37.9276611167, 175.5543011333, \"192\"],\n[-37.9305557833, 175.5594630333, \"129\"],\n[-37.9280362, 175.5517895, \"2/193\"],\n[-37.9284764, 175.5616764333, \"103\"],\n[-37.93143935, 175.55390345, \"165\"],\n[-37.9288132, 175.5647016167, \"61\"],\n[-37.9275235, 175.5619954833, \"94\"],\n[-37.93081245, 175.5577222333, \"149\"],\n[-37.9300416667, 175.5566331333, \"161\"],\n[-37.92921255, 175.5684947, \"16\"],\n[-37.9304111667, 175.5673126333, \"35\"],\n[-37.9291729667, 175.5653647333, \"55\"],\n[-37.9289266333, 175.5656691333, \"56\"],\n[-37.92751825, 175.5531413167, \"1/193\"],\n[-37.9323321667, 175.5512635167, \"1/165\"],\n[-37.9045377667, 175.4827770167, \"3\"],\n[-37.9051343333, 175.4829339167, \"10\"],\n[-37.9045625, 175.4832139167, \"4\"],\n[-37.9052854167, 175.4828661667, \"12\"],\n[-37.9045012833, 175.4825234, \"3A\"],\n[-37.9054383, 175.4831963, \"14\"],\n[-37.9048575167, 175.4826078167, \"7\"],\n[-37.9050790667, 175.4825558167, \"9A\"],\n[-37.90496205, 175.4830191667, \"8\"],\n[-37.9050431833, 175.4823803833, \"9B\"],\n[-37.9047063167, 175.4826914667, \"5\"],\n[-37.9051649333, 175.4825182667, \"11\"],\n[-37.9047697333, 175.4831092667, \"6\"],\n[-37.9044120833, 175.4828407333, \"1\"],\n[-37.8987653333, 175.4845873667, \"12\"],\n[-37.89849935, 175.4843253333, \"8\"],\n[-37.8989808833, 175.4835699333, \"13\"],\n[-37.8982670333, 175.4839818167, \"5\"],\n[-37.89792805, 175.4841291833, \"1\"],\n[-37.8990696333, 175.48395625, \"18\"],\n[-37.8983429167, 175.4837488833, \"7A\"],\n[-37.8986908667, 175.4846387167, \"10\"],\n[-37.8985086333, 175.48330895, \"9A\"],\n[-37.8980756833, 175.4840675333, \"3\"],\n[-37.8984572667, 175.4838707, \"7\"],\n[-37.8988333833, 175.48411825, \"16\"],\n[-37.89797735, 175.4845139167, \"2\"],\n[-37.8988639833, 175.4832473, \"9B\"],\n[-37.8985740667, 175.4844548333, \"8A\"],\n[-37.8983953667, 175.4832022167, \"9\"],\n[-37.898132, 175.48448765, \"4\"],\n[-37.8983169333, 175.48441905, \"6\"],\n[-37.898835, 175.4848386, \"12A\"],\n[-37.8982524667, 175.4837739667, \"5A\"],\n[-37.7992683667, 175.4068494, \"44D\"],\n[-37.7973138333, 175.40680895, \"37\"],\n[-37.79798795, 175.4063265667, \"41\"],\n[-37.7983426667, 175.4060350833, \"38\"],\n[-37.7981863, 175.40728095, \"45\"],\n[-37.7985252667, 175.4070533167, \"46\"],\n[-37.7991710833, 175.4079073667, \"44A\"],\n[-37.7994488333, 175.4084595333, \"44B\"],\n[-37.7998703833, 175.4089765833, \"44C\"],\n[-37.7969343667, 175.4040572333, \"15\"],\n[-37.7991115833, 175.406026, \"44E\"],\n[-37.9108972667, 175.4737860333, \"1\"],\n[-37.9109227833, 175.4740157167, \"3\"],\n[-37.9110122167, 175.47419315, \"5\"],\n[-37.91133475, 175.4740478833, \"6\"],\n[-37.91125305, 175.4738936667, \"4\"],\n[-37.9111422333, 175.4736767667, \"2\"],\n[-37.9110952167, 175.47448295, \"7\"],\n[-37.9112131667, 175.4741245667, \"8\"],\n[-37.8804519333, 175.4809153167, \"16A\"],\n[-37.8808188333, 175.4804471833, \"8\"],\n[-37.8805619667, 175.4804252833, \"10\"],\n[-37.8810451833, 175.4809865167, \"11\"],\n[-37.8806955167, 175.4806394, \"12\"],\n[-37.8808969, 175.48108405, \"13\"],\n[-37.8806416, 175.4807763833, \"14\"],\n[-37.8807567, 175.48110125, \"15\"],\n[-37.8806444167, 175.4809707333, \"16\"],\n[-37.8805414833, 175.4812062167, \"17\"],\n[-37.8812994167, 175.4798864833, \"1\"],\n[-37.8810411333, 175.4798769333, \"2\"],\n[-37.8812669, 175.4801150167, \"3\"],\n[-37.8810092333, 175.4800721333, \"4\"],\n[-37.8811867333, 175.48041535, \"5\"],\n[-37.8809235, 175.4802776833, \"6\"],\n[-37.8811437833, 175.4806999667, \"7\"],\n[-37.8811760167, 175.4808741333, \"9\"],\n[-37.8778504833, 175.5246844167, \"102\"],\n[-37.8819230667, 175.52038225, \"37\"],\n[-37.8838481333, 175.5190955, \"15\"],\n[-37.8825636667, 175.5211268, \"42\"],\n[-37.8789988, 175.5217598167, \"79\"],\n[-37.8729074667, 175.5286693667, \"167\"],\n[-37.8760724333, 175.52502585, \"127\"],\n[-37.8745184833, 175.5278637167, \"148\"],\n[-37.8744641, 175.5268869667, \"145\"],\n[-37.8971449, 175.3967563, \"82\"],\n[-37.8943781833, 175.3977300667, \"44\"],\n[-37.9004613, 175.47640765, \"13\"],\n[-37.9004093667, 175.4762205333, \"11\"],\n[-37.9002790833, 175.4769703167, \"16\"],\n[-37.9003597167, 175.4772565, \"20\"],\n[-37.90090275, 175.4763007167, \"15\"],\n[-37.90094715, 175.4764403167, \"17\"],\n[-37.9006751333, 175.4771785333, \"27\"],\n[-37.90107225, 175.4768541333, \"25\"],\n[-37.9001119667, 175.4749271333, \"1\"],\n[-37.9006160667, 175.4769211667, \"23\"],\n[-37.9002628, 175.4757354, \"3\"],\n[-37.9005473333, 175.4767056, \"19\"],\n[-37.9002167333, 175.4750815167, \"2\"],\n[-37.8998862167, 175.4772123833, \"18\"],\n[-37.9003037333, 175.47596475, \"9\"],\n[-37.9000952167, 175.4752010833, \"2A\"],\n[-37.9006044167, 175.475943, \"9A\"],\n[-37.9085414, 175.47102025, \"1\"],\n[-37.90831215, 175.4705452333, \"4\"],\n[-37.9085338333, 175.4706769667, \"2\"],\n[-37.90815565, 175.470603, \"5\"],\n[-37.9082244667, 175.4708460167, \"6\"],\n[-37.9083062, 175.4711227667, \"7\"],\n[-37.9084509833, 175.47045035, \"3\"],\n[-37.8911392333, 175.4583220667, \"8\"],\n[-37.891127, 175.4585561667, \"10\"],\n[-37.8912095833, 175.4581187333, \"7\"],\n[-37.8910674833, 175.4575599333, \"2\"],\n[-37.8913220333, 175.4574411833, \"1\"],\n[-37.89159775, 175.4573200333, \"1A\"],\n[-37.8910451833, 175.4580652333, \"6\"],\n[-37.8913072667, 175.4576702667, \"3\"],\n[-37.8913322, 175.4579054333, \"5\"],\n[-37.8910210167, 175.45784865, \"4\"],\n[-37.8853014, 175.4629564833, \"3\"],\n[-37.88554135, 175.4629736, \"2\"],\n[-37.88541785, 175.46296925, \"1\"],\n[-37.9193531833, 175.54385725, \"354\"],\n[-37.9188882667, 175.5420886333, \"355\"],\n[-37.9192738333, 175.5435102833, \"356\"],\n[-37.9192985333, 175.5429392833, \"358\"],\n[-37.9193181167, 175.54233135, \"360\"],\n[-37.9192005, 175.5403558833, \"130\"],\n[-37.9186817333, 175.5404104667, \"109\"],\n[-37.9199342167, 175.5412764833, \"260\"],\n[-37.9193768333, 175.5412782167, \"223\"],\n[-37.91831485, 175.5400403333, \"103\"],\n[-37.91961875, 175.5408546833, \"200\"],\n[-37.9176805, 175.5413459167, \"105\"],\n[-37.9190955, 175.5408870167, \"171\"],\n[-37.9182861833, 175.5408937167, \"107\"],\n[-37.9195153, 175.5433797, \"352\"],\n[-37.92030865, 175.54192075, \"264\"],\n[-37.920463, 175.5417725, \"262\"],\n[-37.9197269667, 175.5432480167, \"350\"],\n[-37.9197592167, 175.5415707667, \"266\"],\n[-37.9195913167, 175.54231935, \"348\"],\n[-37.9183186167, 175.5393124667, \"2\"],\n[-37.9187989667, 175.53988695, \"70\"],\n[-37.9185694, 175.5418133, \"353\"],\n[-37.8994012, 175.3657659333, \"821\"],\n[-37.89971895, 175.3645648833, \"835\"],\n[-37.9139332167, 175.4090271667, \"332\"],\n[-37.8988464667, 175.3659825667, \"828\"],\n[-37.9022981, 175.38067085, \"685\"],\n[-37.9065350333, 175.4018221167, \"434\"],\n[-37.90028885, 175.3798328667, \"697\"],\n[-37.9084945333, 175.4050759167, \"410\"],\n[-37.89922135, 175.3740981833, \"747\"],\n[-37.9232422167, 175.4145657333, \"185\"],\n[-37.9245097667, 175.41242555, \"187\"],\n[-37.9159503333, 175.40778185, \"303\"],\n[-37.9094665, 175.4069157667, \"388\"],\n[-37.9231998333, 175.4170489, \"158\"],\n[-37.9102601, 175.4072221667, \"383\"],\n[-37.9207001, 175.4065603167, \"257\"],\n[-37.9102264, 175.4082195, \"372\"],\n[-37.9217580667, 175.4087488, \"233\"],\n[-37.9021599833, 175.3911428167, \"598\"],\n[-37.9229502667, 175.4127942667, \"197\"],\n[-37.90296435, 175.3924815167, \"583\"],\n[-37.9255960833, 175.4136194333, \"2/187\"],\n[-37.9245176, 175.4278129833, \"59\"],\n[-37.9249067167, 175.4263146667, \"75\"],\n[-37.92534045, 175.4130770333, \"1/187\"],\n[-37.9077678, 175.4038107833, \"424\"],\n[-37.9244162333, 175.4258990667, \"76\"],\n[-37.9237273333, 175.4194401833, \"138\"],\n[-37.9019339833, 175.3879181167, \"625\"],\n[-37.90920365, 175.4053418167, \"397\"],\n[-37.9057407667, 175.39478875, \"540\"],\n[-37.9243174333, 175.4220341833, \"112\"],\n[-37.8992012333, 175.3666729333, \"815\"],\n[-37.9110874833, 175.4102195833, \"360\"],\n[-37.9027096, 175.3913196333, \"591\"],\n[-37.9011183833, 175.38410915, \"655\"],\n[-37.9234701333, 175.4155696333, \"181\"],\n[-37.90254175, 175.3926162167, \"582\"],\n[-37.92450575, 175.4246711167, \"90\"],\n[-37.9242924167, 175.4289432833, \"47\"],\n[-37.8986079833, 175.3685293333, \"801\"],\n[-37.9030857, 175.3932839, \"577\"],\n[-37.90235535, 175.3894401667, \"613\"],\n[-37.9008578833, 175.3826145667, \"675\"],\n[-37.90071405, 175.3818195, \"681\"],\n[-37.8820639667, 175.4856738333, \"4\"],\n[-37.8811382833, 175.4847224333, \"17\"],\n[-37.8820705, 175.4859065167, \"2\"],\n[-37.8822594167, 175.4854946333, \"5\"],\n[-37.88230695, 175.4860176667, \"1\"],\n[-37.8816572833, 175.4846057667, \"14\"],\n[-37.8822931167, 175.4857413833, \"3\"],\n[-37.8820614833, 175.4849636833, \"10\"],\n[-37.8814784167, 175.4853259333, \"20\"],\n[-37.8820341167, 175.4854254, \"6\"],\n[-37.8814562667, 175.4855579, \"22\"],\n[-37.8820407667, 175.4852060167, \"8\"],\n[-37.88139725, 175.4857370167, \"24\"],\n[-37.8819474, 175.4846312, \"12\"],\n[-37.8812179833, 175.4855291833, \"23\"],\n[-37.8811665833, 175.4849644, \"19\"],\n[-37.8822871333, 175.4850344167, \"9\"],\n[-37.8822664667, 175.4852611, \"7\"],\n[-37.8813914667, 175.4847524, \"16\"],\n[-37.8812347667, 175.4852638167, \"21\"],\n[-37.8814556, 175.48509055, \"18\"],\n[-37.8811484833, 175.4844946, \"15\"],\n[-37.8823244833, 175.4848154333, \"11\"],\n[-37.8823452833, 175.4845833667, \"13\"],\n[-37.9599893167, 175.5018972167, \"82\"],\n[-37.9618358833, 175.4874459667, \"18\"],\n[-37.9618619, 175.50776785, \"90\"],\n[-37.9616283333, 175.4929460167, \"64\"],\n[-37.9611726167, 175.4984393667, \"80\"],\n[-37.9607851, 175.5016190333, \"86\"],\n[-37.9608416167, 175.4971466, \"78\"],\n[-37.9614436333, 175.5080607667, \"92\"],\n[-37.9643050333, 175.4953529167, \"2/84\"],\n[-37.9610803, 175.4864609, \"7\"],\n[-37.9606146667, 175.4939399, \"83\"],\n[-37.9609926167, 175.4857235333, \"3\"],\n[-37.96113465, 175.4948554, \"84\"],\n[-37.9613254333, 175.5047791833, \"88\"],\n[-37.8593059833, 175.5330650333, \"10\"],\n[-37.8596072333, 175.533587, \"19\"],\n[-37.90423375, 175.4844148, \"107B\"],\n[-37.9020309333, 175.4769959167, \"49A\"],\n[-37.9029281167, 175.4805014167, \"81A\"],\n[-37.9016197667, 175.4756437833, \"37\"],\n[-37.90101005, 175.4735379833, \"21\"],\n[-37.9016823667, 175.4760847833, \"39A\"],\n[-37.90178185, 175.4761837333, \"41\"],\n[-37.9011922667, 175.4725514167, \"8\"],\n[-37.9015593833, 175.4738315333, \"26\"],\n[-37.9015446833, 175.473388, \"18A\"],\n[-37.9024291, 175.4783928, \"57\"],\n[-37.9010319, 175.4736316167, \"23\"],\n[-37.9039576333, 175.4835641667, \"103\"],\n[-37.9011953333, 175.4741573333, \"29A\"],\n[-37.9042121, 175.4828802833, \"100\"],\n[-37.9010152, 175.4741578667, \"29B\"],\n[-37.9019761833, 175.4752665167, \"34\"],\n[-37.90256225, 175.4788024667, \"71\"],\n[-37.9031558833, 175.4793385, \"68\"],\n[-37.9043412833, 175.48477025, \"109A-109D\"],\n[-37.9030168667, 175.4803624833, \"81\"],\n[-37.9054432333, 175.48740955, \"114\"],\n[-37.9032955667, 175.4821555167, \"93B\"],\n[-37.9052043, 175.4875160667, \"118\"],\n[-37.9040282167, 175.4838154167, \"105\"],\n[-37.90498365, 175.4875592167, \"120\"],\n[-37.90388715, 175.4833244, \"101\"],\n[-37.90156105, 175.47306285, \"1/14-5/14\"],\n[-37.9028688, 175.4798400167, \"77\"],\n[-37.9017512, 175.4730746667, \"16A\"],\n[-37.9034353833, 175.48239905, \"95A\"],\n[-37.9018501333, 175.47294875, \"16B\"],\n[-37.9065120667, 175.4873521833, \"114A\"],\n[-37.9027523, 175.47799015, \"58\"],\n[-37.9029325833, 175.4801074833, \"79\"],\n[-37.9032525167, 175.4811784, \"87\"],\n[-37.9031822833, 175.4809204, \"85\"],\n[-37.9033394667, 175.4814522333, \"89\"],\n[-37.9011077167, 175.4738625833, \"25\"],\n[-37.9024641, 175.4756984333, \"40\"],\n[-37.9044449, 175.4836965167, \"104\"],\n[-37.9023471833, 175.47810245, \"55\"],\n[-37.9029669667, 175.4787094167, \"62\"],\n[-37.90442275, 175.4850982, \"115\"],\n[-37.9026089, 175.47587495, \"44\"],\n[-37.90264365, 175.4790681667, \"73\"],\n[-37.9035878333, 175.4823534, \"95\"],\n[-37.9024915, 175.4786270833, \"67\"],\n[-37.9031010167, 175.4812452667, \"87A\"],\n[-37.9030982, 175.47913305, \"66\"],\n[-37.9035099167, 175.4821307, \"93A\"],\n[-37.90079465, 175.4741973833, \"33\"],\n[-37.9040185667, 175.48229025, \"96\"],\n[-37.9023384167, 175.4765524833, \"50\"],\n[-37.9022154167, 175.4786401833, \"59\"],\n[-37.9018837333, 175.4765543333, \"45\"],\n[-37.9029163333, 175.4785064167, \"60\"],\n[-37.9022748167, 175.4763478167, \"48\"],\n[-37.9041422, 175.4826398833, \"98\"],\n[-37.9022017333, 175.4761212333, \"46A\"],\n[-37.9034542333, 175.4818904, \"91\"],\n[-37.9019505833, 175.4767386667, \"47\"],\n[-37.9018310833, 175.4763711167, \"43\"],\n[-37.9022245, 175.4761991, \"46B\"],\n[-37.9022058833, 175.4776710667, \"51\"],\n[-37.9011141167, 175.4738904667, \"31\"],\n[-37.90381365, 175.48307595, \"99\"],\n[-37.9013508667, 175.4731568833, \"14\"],\n[-37.9026744667, 175.4776618667, \"54\"],\n[-37.9013012667, 175.4729456, \"12\"],\n[-37.9016709167, 175.4758279833, \"39\"],\n[-37.9012509333, 175.4727291833, \"10\"],\n[-37.902262, 175.4778573167, \"53\"],\n[-37.9011626167, 175.4723885667, \"6\"],\n[-37.9015607167, 175.4753714333, \"35\"],\n[-37.9007910667, 175.47417145, \"27\"],\n[-37.9020738833, 175.47565455, \"36\"],\n[-37.9010502667, 175.4719555, \"4\"],\n[-37.9036689667, 175.4826166, \"97\"],\n[-37.90165275, 175.4742215833, \"28\"],\n[-37.9043576, 175.4834127, \"102\"],\n[-37.9014172833, 175.4734044167, \"18\"],\n[-37.90310965, 175.4806441333, \"83\"],\n[-37.90305305, 175.4810779, \"85A\"],\n[-37.9041825833, 175.4842419667, \"107A\"],\n[-37.9018864667, 175.4749266667, \"32\"],\n[-37.9018503333, 175.47696095, \"49\"],\n[-37.9030326167, 175.47892515, \"64\"],\n[-37.9017621333, 175.4733169667, \"20\"],\n[-37.89705485, 175.4732848667, \"5\"],\n[-37.8972060833, 175.4727957833, \"1A\"],\n[-37.8950392167, 175.47373745, \"28B\"],\n[-37.8965514333, 175.4726139833, \"6\"],\n[-37.8952181167, 175.4744414833, \"33B\"],\n[-37.8952637167, 175.47354965, \"26A\"],\n[-37.8951086667, 175.4742331167, \"33A\"],\n[-37.8966904333, 175.4729455, \"11A\"],\n[-37.8962754333, 175.4733100167, \"17\"],\n[-37.8968641333, 175.4728138667, \"1/3-5/3\"],\n[-37.8949580667, 175.4743396167, \"35\"],\n[-37.8973106167, 175.4729746, \"1B\"],\n[-37.8949376167, 175.47388345, \"30\"],\n[-37.89572315, 175.4732895, \"18\"],\n[-37.8958303167, 175.4731749333, \"16\"],\n[-37.8969656167, 175.4732634, \"7\"],\n[-37.896495, 175.4731159833, \"13\"],\n[-37.8968125667, 175.4731199333, \"11B\"],\n[-37.89640605, 175.4732035667, \"15\"],\n[-37.8952445667, 175.4736735667, \"26\"],\n[-37.8960091167, 175.47303035, \"14\"],\n[-37.896205, 175.47289145, \"10\"],\n[-37.8956152, 175.4738212833, \"29\"],\n[-37.8950829667, 175.4737896, \"28A\"],\n[-37.8955509167, 175.4734385333, \"22\"],\n[-37.8972144833, 175.4729756, \"1C\"],\n[-37.8970069167, 175.4727237833, \"1D\"],\n[-37.8964112667, 175.4727309333, \"8\"],\n[-37.89685155, 175.4723649167, \"2\"],\n[-37.8959909833, 175.4735371333, \"19\"],\n[-37.8968653333, 175.4732429833, \"9\"],\n[-37.8952936, 175.4740853667, \"31\"],\n[-37.8956491833, 175.47335855, \"20\"],\n[-37.8971588833, 175.4725988, \"1\"],\n[-37.95948505, 175.3813743167, \"3/362\"],\n[-37.9267924667, 175.3947664833, \"790\"],\n[-37.95374205, 175.3785110333, \"417\"],\n[-37.9270906333, 175.3962139333, \"802\"],\n[-37.95208085, 175.3790772833, \"435\"],\n[-37.9537863833, 175.37916715, \"418\"],\n[-37.9336977167, 175.3872475333, \"660\"],\n[-37.9370890667, 175.3861055333, \"610\"],\n[-37.9632177167, 175.37745245, \"310\"],\n[-37.9552044167, 175.3775981, \"391\"],\n[-37.95356905, 175.3785904167, \"419\"],\n[-37.9658669167, 175.3737947167, \"273\"],\n[-37.9591853, 175.37834395, \"360\"],\n[-37.95818485, 175.3776341167, \"1/362\"],\n[-37.9264044, 175.3933694833, \"770\"],\n[-37.94279195, 175.3830579333, \"541\"],\n[-37.9358610167, 175.3856405, \"623\"],\n[-37.92604605, 175.39188825, \"762\"],\n[-37.95893715, 175.3798225167, \"2/362\"],\n[-37.9257693333, 175.3904065167, \"750\"],\n[-37.9654268167, 175.3769618333, \"308\"],\n[-37.9323014833, 175.3868321333, \"743\"],\n[-37.93764955, 175.3850314, \"599\"],\n[-37.9095889, 175.4694829333, \"13\"],\n[-37.9099912333, 175.4694063167, \"4\"],\n[-37.9101332, 175.4693430167, \"2\"],\n[-37.90943375, 175.4691617333, \"9\"],\n[-37.90960705, 175.46916755, \"7\"],\n[-37.9100405, 175.4689760667, \"1\"],\n[-37.9095270167, 175.4693311, \"11\"],\n[-37.9098948167, 175.4690437, \"3\"],\n[-37.9097340167, 175.4696952667, \"10\"],\n[-37.9095571, 175.4697117833, \"12\"],\n[-37.90975285, 175.4691024667, \"5\"],\n[-37.9018515667, 175.47956045, \"10\"],\n[-37.9025733333, 175.4796073833, \"1A\"],\n[-37.90234615, 175.4792779, \"4\"],\n[-37.9027908167, 175.4795601333, \"1\"],\n[-37.9024103667, 175.47967745, \"3\"],\n[-37.90205615, 175.4794369167, \"8\"],\n[-37.89827985, 175.4664433333, \"25\"],\n[-37.8990345167, 175.4670508, \"11\"],\n[-37.8989251667, 175.4664513667, \"17\"],\n[-37.8990204833, 175.4665540333, \"15\"],\n[-37.8996123667, 175.4678836167, \"10\"],\n[-37.89873215, 175.4672828, \"18\"],\n[-37.89858465, 175.4666028667, \"21\"],\n[-37.8990743833, 175.4675788, \"12A\"],\n[-37.8984592, 175.4665057, \"23\"],\n[-37.89889965, 175.467451, \"12\"],\n[-37.9002081667, 175.46756315, \"1\"],\n[-37.9001357167, 175.4679316667, \"4\"],\n[-37.898277, 175.46726385, \"24\"],\n[-37.89978315, 175.46793515, \"8\"],\n[-37.8983929667, 175.4669553667, \"26\"],\n[-37.8991940667, 175.4672154167, \"9\"],\n[-37.8980936167, 175.4664998167, \"27\"],\n[-37.8995042, 175.4674747167, \"7\"],\n[-37.898334, 175.46731845, \"22\"],\n[-37.8999632833, 175.4679440333, \"6\"],\n[-37.89794665, 175.4665422333, \"29\"],\n[-37.8988821167, 175.4669019167, \"13\"],\n[-37.8980275167, 175.4668801333, \"30\"],\n[-37.89871525, 175.4676048667, \"14\"],\n[-37.8977558667, 175.4665362333, \"31\"],\n[-37.8986535, 175.4675602667, \"16\"],\n[-37.8979195833, 175.4667739667, \"32\"],\n[-37.89979585, 175.4675336833, \"5\"],\n[-37.8974943167, 175.46652485, \"33\"],\n[-37.8987147333, 175.46672835, \"19\"],\n[-37.8977257333, 175.4666500833, \"34\"],\n[-37.8985690333, 175.4671123833, \"20\"],\n[-37.9003081667, 175.46791995, \"2\"],\n[-37.8981989167, 175.4668991, \"28\"],\n[-37.8999918333, 175.4675733167, \"3\"],\n[-37.9085979, 175.47228295, \"6\"],\n[-37.9082979333, 175.4727952, \"1\"],\n[-37.9084415, 175.4723396833, \"4\"],\n[-37.9085185333, 175.4726990667, \"3\"],\n[-37.90870765, 175.4724272833, \"8\"],\n[-37.9082453167, 175.4725341667, \"2\"],\n[-37.9087162, 175.4725830333, \"5\"],\n[-37.8992527333, 175.46141755, \"5A\"],\n[-37.8974537333, 175.4597499, \"30\"],\n[-37.8990513167, 175.461357, \"5\"],\n[-37.89854655, 175.4618827833, \"2\"],\n[-37.8962418167, 175.4614646667, \"69\"],\n[-37.89729515, 175.4597868833, \"32\"],\n[-37.8986072, 175.4598125833, \"18\"],\n[-37.8990041, 175.4601224, \"17\"],\n[-37.8986801, 175.46007825, \"16\"],\n[-37.8976013667, 175.4596983333, \"28\"],\n[-37.8987992333, 175.4607934667, \"10\"],\n[-37.8961486, 175.4612296167, \"65\"],\n[-37.8987145333, 175.4602998667, \"14\"],\n[-37.8986250667, 175.4616027833, \"6\"],\n[-37.8985372, 175.4606703, \"12A\"],\n[-37.8990944833, 175.4618171167, \"3A\"],\n[-37.8987547833, 175.4605145833, \"12\"],\n[-37.8989327667, 175.46174375, \"3\"],\n[-37.89940475, 175.4614229833, \"5B\"],\n[-37.8981109833, 175.4595815167, \"22\"],\n[-37.8992761667, 175.4612548833, \"7A\"],\n[-37.89858635, 175.4617420167, \"4\"],\n[-37.89615355, 175.4610093667, \"63\"],\n[-37.8960633167, 175.4614357, \"67\"],\n[-37.8970480833, 175.4594701667, \"47\"],\n[-37.8964719167, 175.4610726, \"46\"],\n[-37.89942805, 175.4612639, \"7B\"],\n[-37.8969129333, 175.4595048833, \"49\"],\n[-37.8983810333, 175.4595983, \"20\"],\n[-37.8966503, 175.4601548, \"40\"],\n[-37.8965228167, 175.4605625667, \"42\"],\n[-37.8979410167, 175.4596252, \"24\"],\n[-37.89698365, 175.45986105, \"36\"],\n[-37.89909445, 175.46094265, \"9\"],\n[-37.8990747, 175.4611943333, \"7\"],\n[-37.8968194333, 175.4598978167, \"38\"],\n[-37.8990957833, 175.4607578667, \"11\"],\n[-37.8993121167, 175.4605309833, \"13A\"],\n[-37.8993347667, 175.4606526667, \"11A\"],\n[-37.8977427333, 175.4596749667, \"26\"],\n[-37.8964422833, 175.4608866833, \"44\"],\n[-37.897148, 175.4598171667, \"34\"],\n[-37.8964457667, 175.4614839667, \"50\"],\n[-37.8965226, 175.4613232333, \"48\"],\n[-37.89666225, 175.4616268167, \"50A\"],\n[-37.8967215667, 175.4614938667, \"48A\"],\n[-37.8961699833, 175.4606764833, \"61\"],\n[-37.8990274167, 175.4594845333, \"21A\"],\n[-37.8978044833, 175.4592938, \"37\"],\n[-37.89765145, 175.45932785, \"39\"],\n[-37.8972011167, 175.4594360833, \"45\"],\n[-37.89748945, 175.45936165, \"41\"],\n[-37.8973585333, 175.4594078333, \"43\"],\n[-37.8989135833, 175.4593674167, \"23A\"],\n[-37.8982502167, 175.4591910333, \"31\"],\n[-37.8980929167, 175.4592136833, \"33\"],\n[-37.8979576667, 175.4592540667, \"35\"],\n[-37.8987865, 175.459442, \"23\"],\n[-37.8983624667, 175.4591781333, \"29\"],\n[-37.89859365, 175.4592542667, \"25\"],\n[-37.8984648833, 175.4592043833, \"27\"],\n[-37.89599285, 175.4605853833, \"61A\"],\n[-37.8967193667, 175.4595488167, \"51\"],\n[-37.89895945, 175.45991735, \"19\"],\n[-37.8988903667, 175.4596039833, \"21\"],\n[-37.8991762833, 175.4598346833, \"19A\"],\n[-37.8990309833, 175.4603212, \"15\"],\n[-37.8992306667, 175.4600061667, \"17A\"],\n[-37.8990798333, 175.46053775, \"13\"],\n[-37.8960234, 175.46042255, \"59A\"],\n[-37.8961819167, 175.4595465333, \"55A\"],\n[-37.8965562167, 175.4596334167, \"53\"],\n[-37.8962467833, 175.4604475, \"59\"],\n[-37.8961687167, 175.459936, \"57A\"],\n[-37.8963601167, 175.4599881167, \"57\"],\n[-37.8964266, 175.4598209333, \"55\"],\n[-37.89876895, 175.4610164333, \"8\"],\n[-37.89888655, 175.46191985, \"1\"],\n[-37.8992657, 175.4618445667, \"3B\"],\n[-37.89942775, 175.46177925, \"3C\"],\n[-37.8994213333, 175.461939, \"3D\"],\n[-37.8877845667, 175.4769104167, \"15A\"],\n[-37.8884691333, 175.47652905, \"9\"],\n[-37.8876236833, 175.4762465333, \"14\"],\n[-37.8881092667, 175.4765359833, \"11\"],\n[-37.8870282333, 175.47689615, \"21\"],\n[-37.8878168667, 175.4761862, \"12\"],\n[-37.8869641167, 175.4766908333, \"20\"],\n[-37.88876965, 175.4761694167, \"7\"],\n[-37.8866340667, 175.4762948167, \"20D\"],\n[-37.8875209833, 175.4767726667, \"17\"],\n[-37.8878835333, 175.4766054, \"13\"],\n[-37.8870823167, 175.4763955, \"18\"],\n[-37.8885070833, 175.4758756667, \"6\"],\n[-37.8880108, 175.4760921833, \"10\"],\n[-37.8888735667, 175.4758855333, \"5\"],\n[-37.8879635, 175.4768245333, \"13A\"],\n[-37.8890102, 175.4753934667, \"1\"],\n[-37.887397, 175.4763382, \"16\"],\n[-37.8867165333, 175.4763817833, \"20C\"],\n[-37.8881819, 175.4760513167, \"8\"],\n[-37.886801, 175.4764408667, \"20B\"],\n[-37.8873439167, 175.4769787167, \"19A\"],\n[-37.88688675, 175.47651985, \"20A\"],\n[-37.8872187, 175.4769650667, \"19\"],\n[-37.8874603333, 175.4770461667, \"19B\"],\n[-37.8877107667, 175.4766732833, \"15\"],\n[-37.9028793667, 175.4694345, \"9\"],\n[-37.9027742, 175.4691508167, \"5\"],\n[-37.9027706333, 175.46934705, \"7\"],\n[-37.9030670833, 175.4689651833, \"4\"],\n[-37.903068, 175.4692231333, \"6\"],\n[-37.90282975, 175.4689251167, \"2\"],\n[-37.9030033, 175.4694327333, \"8\"],\n[-37.7979216167, 175.3709623333, \"40\"],\n[-37.7901529, 175.3755824, \"144\"],\n[-37.7907598333, 175.3744419, \"134\"],\n[-37.7937929667, 175.3664199167, \"59C\"],\n[-37.79439395, 175.3674624167, \"59B\"],\n[-37.7944289667, 175.3725634167, \"82A\"],\n[-37.7904513167, 175.3738425333, \"131\"],\n[-37.794607, 175.3745403833, \"82B\"],\n[-37.7921551167, 175.3768403, \"132\"],\n[-37.7926399833, 175.3722763, \"102\"],\n[-37.7967557, 175.3729791333, \"60A\"],\n[-37.7921326333, 175.37322975, \"110\"],\n[-37.8012874, 175.3703331, \"14\"],\n[-37.7948232167, 175.3716246, \"74\"],\n[-37.7979944667, 175.3703715167, \"35\"],\n[-37.7956465, 175.3714636167, \"66\"],\n[-37.7911951833, 175.3732412833, \"123B\"],\n[-37.7987651833, 175.3708161667, \"32\"],\n[-37.79171095, 175.3736367667, \"112\"],\n[-37.7976371833, 175.3704513833, \"41\"],\n[-37.7911877333, 175.3732945667, \"123A\"],\n[-37.7913029167, 175.37306835, \"121\"],\n[-37.7906186667, 175.3755869, \"140\"],\n[-37.7931244667, 175.3712613167, \"93A\"],\n[-37.7974290667, 175.3711039667, \"42\"],\n[-37.7936148167, 175.3719375667, \"84\"],\n[-37.8005517667, 175.3700387, \"9\"],\n[-37.7938986667, 175.3710440333, \"77\"],\n[-37.7965492333, 175.3713085167, \"54\"],\n[-37.8001176167, 175.3701064333, \"13\"],\n[-37.7930191667, 175.37129215, \"93B\"],\n[-37.7926151667, 175.37155955, \"99A\"],\n[-37.7950692833, 175.3686159167, \"59A\"],\n[-37.7969088, 175.37462925, \"60B\"],\n[-37.7951773167, 175.37087185, \"75\"],\n[-37.7921505, 175.3713529333, \"99B\"],\n[-37.7904565167, 175.3749809333, \"138\"],\n[-37.8006982167, 175.3700011, \"7\"],\n[-37.7991912, 175.3699700833, \"29\"],\n[-37.7967847667, 175.3705662167, \"47\"],\n[-37.792296, 175.3720602333, \"99C\"],\n[-37.7980087167, 175.36967245, \"31\"],\n[-37.7982152833, 175.3709274, \"34\"],\n[-37.9155702167, 175.4723301167, \"4\"],\n[-37.9159029, 175.4721897167, \"8\"],\n[-37.9157610833, 175.4726929667, \"3\"],\n[-37.9160956333, 175.4725375333, \"7\"],\n[-37.9153328, 175.4724436667, \"2\"],\n[-37.9164312667, 175.4723669167, \"11\"],\n[-37.9165995, 175.4722833833, \"13\"],\n[-37.9162671833, 175.4724523333, \"9\"],\n[-37.9162124833, 175.4720273, \"12\"],\n[-37.9160704833, 175.4720950333, \"10\"],\n[-37.9170187667, 175.4717177667, \"22\"],\n[-37.9164956667, 175.4718994167, \"16\"],\n[-37.9163615667, 175.4719599333, \"14\"],\n[-37.9155396833, 175.4728079667, \"1\"],\n[-37.9157325833, 175.4722579833, \"6\"],\n[-37.9159316, 175.47261835, \"5\"],\n[-37.8914888167, 175.4620999667, \"21\"],\n[-37.8837548, 175.4623328, \"77B\"],\n[-37.8930618833, 175.4622745, \"11\"],\n[-37.8852888667, 175.46116795, \"76\"],\n[-37.8913069667, 175.4621727167, \"23\"],\n[-37.8839160167, 175.4615298167, \"75\"],\n[-37.8883285167, 175.4618738833, \"47\"],\n[-37.8858521667, 175.46093185, \"68B\"],\n[-37.8853005333, 175.4610670333, \"76A\"],\n[-37.88383285, 175.4621270167, \"75A\"],\n[-37.8823235167, 175.4605504833, \"110A\"],\n[-37.8914899333, 175.46174725, \"22\"],\n[-37.8818567333, 175.4608746167, \"116\"],\n[-37.88353235, 175.4618320333, \"77A\"],\n[-37.88134175, 175.4613161333, \"95\"],\n[-37.8879809, 175.4618373667, \"49\"],\n[-37.88152115, 175.46131485, \"93\"],\n[-37.8885148667, 175.4618976167, \"45\"],\n[-37.8872325667, 175.4613256333, \"54\"],\n[-37.8877582, 175.4620199, \"51A\"],\n[-37.8837208833, 175.4615252, \"77\"],\n[-37.8879695167, 175.4611445833, \"50A\"],\n[-37.8933130333, 175.46192685, \"6\"],\n[-37.88794035, 175.46146015, \"50\"],\n[-37.8911399667, 175.4621445, \"25\"],\n[-37.8927127, 175.4625991167, \"15\"],\n[-37.8898722833, 175.4620120667, \"37\"],\n[-37.8817498333, 175.4605830833, \"118A\"],\n[-37.8856498167, 175.4611874667, \"70\"],\n[-37.8818152333, 175.4605837833, \"118B\"],\n[-37.8834546167, 175.4619323333, \"1/79-3/79\"],\n[-37.8817077833, 175.46087495, \"120\"],\n[-37.88566165, 175.4608763667, \"70B\"],\n[-37.8881064833, 175.4614714667, \"48\"],\n[-37.8883276833, 175.4614909, \"46\"],\n[-37.8900726, 175.4616307167, \"34\"],\n[-37.8824921167, 175.4614109333, \"85\"],\n[-37.8902574, 175.4620445833, \"33\"],\n[-37.8821857333, 175.4613799, \"89\"],\n[-37.8909901, 175.4620973667, \"27\"],\n[-37.8906249333, 175.4624066333, \"31A\"],\n[-37.8909031667, 175.4623998167, \"27A\"],\n[-37.8823715333, 175.4609577, \"110\"],\n[-37.8885618333, 175.4615104, \"44\"],\n[-37.8907907, 175.4624375833, \"29A\"],\n[-37.8911051167, 175.4615235333, \"26A\"],\n[-37.8912843667, 175.46171725, \"24\"],\n[-37.88775155, 175.4618187333, \"51\"],\n[-37.88202465, 175.4608939, \"114\"],\n[-37.8877833167, 175.4614137, \"52\"],\n[-37.8907810833, 175.462108, \"29\"],\n[-37.8910314667, 175.4617401833, \"26\"],\n[-37.882292, 175.4613977333, \"87\"],\n[-37.8906141333, 175.4621064667, \"31\"],\n[-37.8892757667, 175.4615758167, \"38\"],\n[-37.8904495, 175.4624898833, \"33B\"],\n[-37.8868235333, 175.461296, \"58\"],\n[-37.8904039167, 175.4622297, \"33A\"],\n[-37.8826623167, 175.4609461833, \"106\"],\n[-37.89295465, 175.4615430167, \"14B\"],\n[-37.8907016, 175.46165145, \"28\"],\n[-37.89291045, 175.4618812167, \"14\"],\n[-37.8854757, 175.461181, \"74\"],\n[-37.8930916667, 175.46189575, \"12\"],\n[-37.8856999, 175.4605845, \"70A\"],\n[-37.8928368667, 175.4622630833, \"13\"],\n[-37.8899615833, 175.4616178167, \"36\"],\n[-37.8925096667, 175.4618395167, \"16A\"],\n[-37.8927205667, 175.4618461, \"16\"],\n[-37.8925576333, 175.4622484333, \"17\"],\n[-37.8887964167, 175.4615352, \"42\"],\n[-37.8887897667, 175.4619157667, \"43\"],\n[-37.8813160833, 175.46082205, \"124\"],\n[-37.88148945, 175.46084125, \"122\"],\n[-37.8822301, 175.4605952667, \"112B\"],\n[-37.8821928667, 175.4609133167, \"112A\"],\n[-37.8825255, 175.4609447167, \"108\"],\n[-37.886356, 175.4607633833, \"62A\"],\n[-37.88384855, 175.46107535, \"84\"],\n[-37.8844810833, 175.46111955, \"78\"],\n[-37.8865957833, 175.4612863167, \"60\"],\n[-37.8870465333, 175.4613338333, \"56\"],\n[-37.8840136833, 175.4610932, \"82\"],\n[-37.8840612333, 175.4615617667, \"73\"],\n[-37.8858403667, 175.4612416667, \"68A\"],\n[-37.8863992833, 175.4612464333, \"62\"],\n[-37.88602145, 175.4612203, \"66\"],\n[-37.8862464, 175.4607918667, \"64A\"],\n[-37.8859472, 175.4606057333, \"66A\"],\n[-37.88617885, 175.4612264667, \"64\"],\n[-37.8841155833, 175.4606926667, \"82A\"],\n[-37.8842170667, 175.46156725, \"71\"],\n[-37.8843494167, 175.4615815167, \"69\"],\n[-37.8845482333, 175.4615789, \"61\"],\n[-37.8832313667, 175.4614822, \"81\"],\n[-37.8833854333, 175.46147585, \"79\"],\n[-37.8842273333, 175.4610982667, \"80\"],\n[-37.8842230833, 175.4607088333, \"80A\"],\n[-37.88898595, 175.4619261, \"41\"],\n[-37.8836924167, 175.4610583833, \"86\"],\n[-37.88897595, 175.4615556167, \"40\"],\n[-37.8835485833, 175.4610374, \"88\"],\n[-37.8812029, 175.4608328833, \"126\"],\n[-37.8834193333, 175.4610251833, \"90\"],\n[-37.8174314833, 175.3761889833, \"28\"],\n[-37.8175173333, 175.3745060667, \"41A\"],\n[-37.8189801, 175.3767644333, \"19\"],\n[-37.81721685, 175.3746944667, \"40A\"],\n[-37.8184217833, 175.3785756333, \"7\"],\n[-37.81727655, 175.3739209, \"41C\"],\n[-37.8169354333, 175.3751527667, \"40B\"],\n[-37.8183142667, 175.3745782, \"39\"],\n[-37.8177220333, 175.3774063833, \"18\"],\n[-37.8181192333, 175.3734650333, \"41B\"],\n[-37.8182567, 175.37774445, \"15\"],\n[-37.8177787833, 175.3756114, \"31\"],\n[-37.8171698, 175.37896375, \"14\"],\n[-37.8179103833, 175.3762068167, \"27\"],\n[-37.81816045, 175.3773152167, \"17\"],\n[-37.8187562833, 175.3765890167, \"21\"],\n[-37.81804885, 175.3788571, \"6\"],\n[-37.8184971667, 175.3750758833, \"49\"],\n[-37.8173303, 175.3796556667, \"12\"],\n[-37.8177653667, 175.3776830167, \"16\"],\n[-37.81610655, 175.3744603333, \"40C\"],\n[-37.8175659833, 175.3767630167, \"24\"],\n[-37.8857427167, 175.4668167833, \"6\"],\n[-37.8842291667, 175.46713325, \"21\"],\n[-37.88552105, 175.46728075, \"7\"],\n[-37.8822557833, 175.4671910833, \"41A\"],\n[-37.88573115, 175.4665300333, \"6A\"],\n[-37.8841256167, 175.4667261, \"22\"],\n[-37.8855689, 175.4668563667, \"8\"],\n[-37.8833609167, 175.4664140667, \"30A\"],\n[-37.8819612833, 175.4662849167, \"42A\"],\n[-37.8854916667, 175.4665621167, \"8A\"],\n[-37.88355305, 175.4666830167, \"28\"],\n[-37.8858997833, 175.46683345, \"2\"],\n[-37.88529365, 175.4675717333, \"9A\"],\n[-37.8853087167, 175.46724515, \"9\"],\n[-37.8858362167, 175.4668311, \"4\"],\n[-37.88501735, 175.4672261167, \"13\"],\n[-37.88173245, 175.4665125833, \"44\"],\n[-37.8819903833, 175.4669395333, \"43\"],\n[-37.8819599, 175.46652035, \"42\"],\n[-37.8821801667, 175.4669412, \"41\"],\n[-37.8820951, 175.4665340833, \"40\"],\n[-37.8822636667, 175.4665534167, \"36\"],\n[-37.8836934333, 175.4671131833, \"27\"],\n[-37.8817733333, 175.4669241, \"45\"],\n[-37.8811819833, 175.46709535, \"49A\"],\n[-37.8812991667, 175.4664858167, \"48\"],\n[-37.8815565333, 175.4668967167, \"47\"],\n[-37.8815367, 175.46651585, \"46\"],\n[-37.8837635333, 175.4667121, \"26\"],\n[-37.8811047333, 175.466856, \"51\"],\n[-37.8810985833, 175.4664679167, \"50\"],\n[-37.8813315833, 175.46687885, \"49\"],\n[-37.8838189667, 175.4665006, \"26A\"],\n[-37.8840672667, 175.46711675, \"23\"],\n[-37.8838811667, 175.4670993333, \"25\"],\n[-37.8839458833, 175.4667205667, \"24\"],\n[-37.8832529, 175.4673047167, \"33A\"],\n[-37.88359175, 175.4664604333, \"28A\"],\n[-37.8830642667, 175.4666342167, \"32\"],\n[-37.8835364167, 175.4675321, \"29\"],\n[-37.8834683333, 175.4670743667, \"31\"],\n[-37.883317, 175.4666640833, \"30\"],\n[-37.8851499333, 175.4667468, \"12\"],\n[-37.8851338167, 175.46723935, \"11\"],\n[-37.8828958667, 175.4666283167, \"34\"],\n[-37.88325825, 175.46707095, \"33\"],\n[-37.8853570333, 175.4667731167, \"10\"],\n[-37.88494775, 175.4672146833, \"15\"],\n[-37.8854012333, 175.466548, \"10A\"],\n[-37.88298215, 175.4670794667, \"35\"],\n[-37.9137448167, 175.4769976167, \"9\"],\n[-37.9139995, 175.4769421167, \"1\"],\n[-37.9137195833, 175.4762766333, \"5\"],\n[-37.9136874333, 175.4768144167, \"8\"],\n[-37.9138555833, 175.4762229167, \"4\"],\n[-37.9139553833, 175.4765034, \"3\"],\n[-37.9139429833, 175.4767303, \"2\"],\n[-37.91357485, 175.4763857833, \"6\"],\n[-37.91363055, 175.47658545, \"7\"],\n[-37.9070534333, 175.4673664667, \"26\"],\n[-37.9051596, 175.4668419167, \"13\"],\n[-37.9107910167, 175.4652498167, \"70\"],\n[-37.9049454167, 175.4681745833, \"8\"],\n[-37.9086697833, 175.466192, \"46A\"],\n[-37.9049889333, 175.4680154333, \"8A\"],\n[-37.90506145, 175.4674941833, \"7\"],\n[-37.9106372333, 175.4652995167, \"68\"],\n[-37.9083611333, 175.4663507667, \"42\"],\n[-37.9062177333, 175.4681393167, \"16B\"],\n[-37.91076535, 175.46463305, \"71\"],\n[-37.90610465, 175.46785555, \"16A\"],\n[-37.9054751167, 175.4677389667, \"12\"],\n[-37.9058089833, 175.4680032167, \"14A\"],\n[-37.9054853667, 175.4672854833, \"17\"],\n[-37.90856345, 175.4665805333, \"44\"],\n[-37.9088263, 175.4661217333, \"48\"],\n[-37.9101238833, 175.4655415667, \"62\"],\n[-37.908529, 175.4662483167, \"46\"],\n[-37.9056721833, 175.46797, \"12A\"],\n[-37.9057461833, 175.4675941167, \"14\"],\n[-37.90600145, 175.4670439167, \"23\"],\n[-37.9076178833, 175.4673104167, \"34A\"],\n[-37.90463895, 175.46807945, \"3\"],\n[-37.9049602667, 175.4669330833, \"9\"],\n[-37.90673685, 175.4671040833, \"22\"],\n[-37.907098, 175.4676191667, \"32\"],\n[-37.9076081833, 175.46671515, \"38\"],\n[-37.90916225, 175.4659552667, \"52\"],\n[-37.90454345, 175.4684433, \"1\"],\n[-37.90512805, 175.4671995667, \"11A\"],\n[-37.9059193, 175.4674994, \"16\"],\n[-37.9073133167, 175.4668277667, \"34\"],\n[-37.9102952667, 175.4654520833, \"64\"],\n[-37.9092774833, 175.46591125, \"52A\"],\n[-37.9052165167, 175.4674315333, \"11\"],\n[-37.9053672, 175.4671046, \"17B\"],\n[-37.9048409833, 175.46859545, \"4\"],\n[-37.905671, 175.46720995, \"19\"],\n[-37.9048928333, 175.4683727167, \"6\"],\n[-37.9058366833, 175.4671128, \"21\"],\n[-37.9071612667, 175.4678683, \"30\"],\n[-37.9089840667, 175.4660592667, \"50\"],\n[-37.90527545, 175.4667925333, \"15\"],\n[-37.90715615, 175.466917, \"32A\"],\n[-37.9061779167, 175.4669647333, \"25\"],\n[-37.9077133333, 175.4674576833, \"36A\"],\n[-37.9104661667, 175.4653805167, \"66\"],\n[-37.9051668667, 175.4678703167, \"10\"],\n[-37.89370395, 175.47271155, \"11\"],\n[-37.8921716167, 175.4721017, \"26\"],\n[-37.8935424167, 175.4728224833, \"13A\"],\n[-37.8935212, 175.4727174167, \"13\"],\n[-37.8937888333, 175.4722797667, \"12\"],\n[-37.89444175, 175.4723851667, \"2\"],\n[-37.89426415, 175.4731979833, \"8/7\"],\n[-37.8944458167, 175.4722873333, \"1/2\"],\n[-37.8940236833, 175.4722967667, \"8\"],\n[-37.8944464167, 175.47217745, \"2/2\"],\n[-37.89289535, 175.4722046, \"20\"],\n[-37.8940564, 175.4721777667, \"1/8\"],\n[-37.89387485, 175.47305145, \"9A\"],\n[-37.8941962833, 175.4734008333, \"7/7\"],\n[-37.8940251167, 175.4729672167, \"3/7\"],\n[-37.8940147167, 175.4733695833, \"5/7\"],\n[-37.8927664833, 175.4726649, \"21\"],\n[-37.8940957833, 175.4733820167, \"6/7\"],\n[-37.8942063833, 175.47275275, \"11/7\"],\n[-37.8938730167, 175.4727318667, \"9\"],\n[-37.8940384833, 175.4727499833, \"1/7\"],\n[-37.8916949833, 175.47255425, \"29\"],\n[-37.8940345667, 175.4728609333, \"2/7\"],\n[-37.8920309, 175.4720907167, \"28\"],\n[-37.8942669167, 175.4723162, \"6\"],\n[-37.8920657333, 175.4725813667, \"27\"],\n[-37.8924789, 175.4721364167, \"22\"],\n[-37.89162635, 175.4720620167, \"34\"],\n[-37.89420475, 175.4728706, \"10/7\"],\n[-37.8924279, 175.4726186167, \"25\"],\n[-37.89427175, 175.4730613833, \"9/7\"],\n[-37.8917478833, 175.4720685333, \"32\"],\n[-37.89359425, 175.4722612333, \"14\"],\n[-37.8919238167, 175.4717851167, \"30A\"],\n[-37.89358685, 175.47297855, \"11A\"],\n[-37.8917815667, 175.4717468667, \"32A\"],\n[-37.8940195667, 175.4730878667, \"4/7\"],\n[-37.89188475, 175.4720793833, \"30\"],\n[-37.8766258167, 175.4531757333, \"63\"],\n[-37.8793250667, 175.4531694667, \"33\"],\n[-37.87557575, 175.4531778333, \"71\"],\n[-37.8649285167, 175.4537918667, \"194\"],\n[-37.8758193333, 175.4536709167, \"72\"],\n[-37.8730099333, 175.4536885167, \"104\"],\n[-37.87537995, 175.4536875833, \"76\"],\n[-37.866365, 175.4537908667, \"182\"],\n[-37.8773983, 175.4531370667, \"53\"],\n[-37.8663474333, 175.4540325167, \"182A\"],\n[-37.8765353, 175.4536952667, \"68\"],\n[-37.8633266833, 175.4532383833, \"213\"],\n[-37.8748418833, 175.4536900667, \"86\"],\n[-37.8723979667, 175.4536811667, \"112\"],\n[-37.8789557167, 175.4531799167, \"37\"],\n[-37.8744338333, 175.45543435, \"88\"],\n[-37.8780606333, 175.4531695167, \"49\"],\n[-37.8613876667, 175.4533495833, \"1/233\"],\n[-37.8779126333, 175.449236, \"4/51\"],\n[-37.86132765, 175.4526138667, \"2/233\"],\n[-37.8779974333, 175.45063155, \"3/51\"],\n[-37.8629783167, 175.4532700667, \"215\"],\n[-37.8777104167, 175.4515755167, \"2/51\"],\n[-37.8811745, 175.4555028167, \"4\"],\n[-37.8779778833, 175.4521171, \"1/51\"],\n[-37.8619675333, 175.4532946667, \"227\"],\n[-37.8786639833, 175.4493194167, \"5/51\"],\n[-37.88057795, 175.4544579, \"18\"],\n[-37.86531615, 175.4531001833, \"191\"],\n[-37.8655207167, 175.4536965333, \"186\"],\n[-37.8646614333, 175.4537639667, \"198\"],\n[-37.8647871333, 175.4541422833, \"196\"],\n[-37.86771765, 175.4537268, \"162\"],\n[-37.8711387333, 175.4536879167, \"124\"],\n[-37.8611343167, 175.45330145, \"235\"],\n[-37.8624900167, 175.4532548833, \"221\"],\n[-37.86872715, 175.4532476, \"151\"],\n[-37.8703843667, 175.4532525, \"131\"],\n[-37.8692643833, 175.45371825, \"150\"],\n[-37.8699453833, 175.4537083667, \"142\"],\n[-37.88075845, 175.4546889, \"14\"],\n[-37.8810108833, 175.45510075, \"8\"],\n[-37.8808800833, 175.45486695, \"12\"],\n[-37.8810951, 175.45531175, \"6\"],\n[-37.8812414333, 175.4557114167, \"2\"],\n[-37.8737279833, 175.4537149833, \"90\"],\n[-37.8804315833, 175.4547993833, \"2/16\"],\n[-37.8806571333, 175.4551276833, \"2/10\"],\n[-37.8807296667, 175.4552756333, \"1/10\"],\n[-37.8805078333, 175.4548978667, \"1/16\"],\n[-37.8305417667, 175.4407137167, \"106\"],\n[-37.8305758833, 175.4444438667, \"78\"],\n[-37.8305754, 175.45156825, \"18\"],\n[-37.83063965, 175.45322105, \"2\"],\n[-37.8305728333, 175.4437875667, \"82\"],\n[-37.8690497667, 175.4740093167, \"7\"],\n[-37.86952375, 175.4790699, \"60\"],\n[-37.8713118667, 175.4763685667, \"42\"],\n[-37.8690602667, 175.4767549167, \"47\"],\n[-37.8957648333, 175.4773709167, \"4A\"],\n[-37.8971239167, 175.47716665, \"16\"],\n[-37.89663135, 175.4774846833, \"10\"],\n[-37.8972863333, 175.4770748833, \"18\"],\n[-37.8969474167, 175.4772745667, \"14\"],\n[-37.8963529167, 175.4776375333, \"8\"],\n[-37.8982169667, 175.4746382667, \"44\"],\n[-37.8958142, 175.4771743333, \"4B\"],\n[-37.89577995, 175.4770061667, \"4C\"],\n[-37.8956352667, 175.4772386167, \"4\"],\n[-37.89679325, 175.4773878, \"12\"],\n[-37.8956875333, 175.4769206, \"4D\"],\n[-37.8955368, 175.4771246333, \"2\"],\n[-37.9075869833, 175.4681207167, \"5\"],\n[-37.9076935833, 175.4688911, \"1\"],\n[-37.9078046833, 175.4683225333, \"7\"],\n[-37.9077261333, 175.4681354833, \"6\"],\n[-37.9078473333, 175.4685627833, \"8\"],\n[-37.9075310167, 175.46846565, \"3\"],\n[-37.9079180333, 175.46879245, \"9\"],\n[-37.90762245, 175.4686683, \"2\"],\n[-37.9074934833, 175.4682448, \"4\"],\n[-37.8065385167, 175.3969116167, \"26\"],\n[-37.8052316167, 175.3947853833, \"1\"],\n[-37.8054979333, 175.3951090333, \"5\"],\n[-37.8061355333, 175.3955903667, \"12\"],\n[-37.8057016167, 175.3950258333, \"6\"],\n[-37.8059893667, 175.3953958833, \"10\"],\n[-37.8068311167, 175.3965807, \"22\"],\n[-37.80583815, 175.3952011167, \"8\"],\n[-37.8067129833, 175.3967156, \"24\"],\n[-37.8062816833, 175.39578495, \"14\"],\n[-37.8056595, 175.3953378, \"7\"],\n[-37.80642775, 175.3959857667, \"16\"],\n[-37.8067297167, 175.39640035, \"20\"],\n[-37.8063663, 175.3962931167, \"13\"],\n[-37.8053626333, 175.3949338333, \"3\"],\n[-37.8065366, 175.3965265333, \"15\"],\n[-37.80539065, 175.394573, \"2\"],\n[-37.8065884, 175.39621225, \"18\"],\n[-37.8055506833, 175.39481855, \"4\"],\n[-37.8062047667, 175.3961045, \"11\"],\n[-37.91218215, 175.4768794167, \"8\"],\n[-37.9120288167, 175.4765747, \"7B\"],\n[-37.9122875167, 175.4765784167, \"3\"],\n[-37.9123338833, 175.4771329833, \"6B\"],\n[-37.9120254167, 175.4764380167, \"5\"],\n[-37.91246155, 175.4769255333, \"4\"],\n[-37.9125897333, 175.4768015, \"2\"],\n[-37.9121711333, 175.4767164667, \"7\"],\n[-37.9122827167, 175.4769958667, \"6A\"],\n[-37.8527505833, 175.53393255, \"617\"],\n[-37.8826752167, 175.5025520333, \"9\"],\n[-37.81291845, 175.5447496, \"1161\"],\n[-37.8788362167, 175.510149, \"1/88\"],\n[-37.81259405, 175.5447376, \"1163\"],\n[-37.8763215167, 175.5110032333, \"115\"],\n[-37.8751193167, 175.51276665, \"134\"],\n[-37.86748695, 175.5219444167, \"272\"],\n[-37.8793489167, 175.5093666167, \"80\"],\n[-37.8249454833, 175.5459290333, \"1021\"],\n[-37.87870795, 175.5090378667, \"81\"],\n[-37.8802461667, 175.5019911, \"1/37\"],\n[-37.8790009333, 175.5095332, \"84\"],\n[-37.8675435167, 175.51850165, \"243\"],\n[-37.8783544, 175.5094524833, \"97\"],\n[-37.8785864667, 175.5105412333, \"2/88\"],\n[-37.8565379167, 175.5340265667, \"567\"],\n[-37.8790696167, 175.5117932, \"98\"],\n[-37.8579882167, 175.5332226833, \"557\"],\n[-37.8228546333, 175.5478497667, \"1043\"],\n[-37.8111895167, 175.5446341667, \"1177\"],\n[-37.8670294, 175.5209776667, \"265\"],\n[-37.8111449, 175.5438866, \"1179\"],\n[-37.8810532167, 175.50530935, \"39\"],\n[-37.8107934667, 175.5438973833, \"1183\"],\n[-37.8766940333, 175.51263885, \"2/110\"],\n[-37.8094178833, 175.5439488333, \"1195\"],\n[-37.8484587833, 175.5342090667, \"663\"],\n[-37.81064835, 175.5445837167, \"1187\"],\n[-37.8744863, 175.5115857333, \"139\"],\n[-37.8099838, 175.54453645, \"1191\"],\n[-37.8787071333, 175.5051437667, \"2/39\"],\n[-37.8092644167, 175.54453125, \"1199\"],\n[-37.8361982667, 175.5463852167, \"890\"],\n[-37.8389412667, 175.5464548, \"858\"],\n[-37.8789616667, 175.5121996833, \"100\"],\n[-37.8336715167, 175.5454231, \"919\"],\n[-37.8289189, 175.5478054833, \"986\"],\n[-37.8323492833, 175.5460591, \"928\"],\n[-37.8320521833, 175.5459855, \"930\"],\n[-37.8251348833, 175.5480670667, \"1016\"],\n[-37.8561845167, 175.5327450833, \"581\"],\n[-37.8262207333, 175.5482595333, \"996\"],\n[-37.8083614833, 175.5449377833, \"1211\"],\n[-37.8172147667, 175.5457622333, \"1107\"],\n[-37.8461716667, 175.5348015333, \"688\"],\n[-37.8159882, 175.5454072833, \"1119\"],\n[-37.8801261, 175.5011647833, \"37\"],\n[-37.8788507333, 175.5071659, \"5/39\"],\n[-37.86353225, 175.5262979167, \"392\"],\n[-37.8087557667, 175.5460182, \"1208\"],\n[-37.8799249, 175.50989025, \"70\"],\n[-37.8087391167, 175.5453725167, \"1206\"],\n[-37.8368114333, 175.5449542167, \"881\"],\n[-37.8278101833, 175.5479474667, \"990\"],\n[-37.8144593833, 175.5446729333, \"1143\"],\n[-37.8763691667, 175.51261455, \"1/110\"],\n[-37.8141157667, 175.5446296, \"1145\"],\n[-37.8675668333, 175.5223205667, \"276\"],\n[-37.86875635, 175.5151021667, \"207\"],\n[-37.8785670167, 175.5058547667, \"3/39\"],\n[-37.8766803833, 175.5140811167, \"6/110\"],\n[-37.86696285, 175.5258656667, \"302\"],\n[-37.81135475, 175.5425439833, \"1/1181\"],\n[-37.8809864833, 175.5057107, \"8/39\"],\n[-37.8788209, 175.5065659833, \"4/39\"],\n[-37.8792938833, 175.5065692167, \"7/39\"],\n[-37.8139674667, 175.5456240667, \"1146\"],\n[-37.8611906333, 175.5269748, \"432\"],\n[-37.8402324, 175.5411414667, \"809\"],\n[-37.82010205, 175.5480512333, \"1070\"],\n[-37.8771378833, 175.51029235, \"1/103\"],\n[-37.8213269333, 175.54882125, \"1052\"],\n[-37.8089942, 175.5452704833, \"1202\"],\n[-37.8212039667, 175.5500653333, \"1/1052\"],\n[-37.86704345, 175.5199686167, \"257\"],\n[-37.8211860833, 175.55188125, \"2/1052\"],\n[-37.8732234167, 175.51193365, \"151\"],\n[-37.8231706667, 175.5483519, \"1040\"],\n[-37.8129427333, 175.54544965, \"1158\"],\n[-37.8398410833, 175.5429798, \"817\"],\n[-37.8774619667, 175.5081401333, \"4/103\"],\n[-37.8413094333, 175.5414880333, \"796\"],\n[-37.8734242333, 175.5114266833, \"149\"],\n[-37.8116329167, 175.54465065, \"1173\"],\n[-37.8790814667, 175.505419, \"6/39\"],\n[-37.8402537, 175.54399925, \"830\"],\n[-37.87415495, 175.5111152667, \"1/139\"],\n[-37.8090223833, 175.5446570333, \"1201\"],\n[-37.8809561333, 175.5036574833, \"35\"],\n[-37.8085440167, 175.5454961667, \"1210\"],\n[-37.8413132, 175.5395747333, \"773\"],\n[-37.87539025, 175.5146704167, \"1/138\"],\n[-37.83847385, 175.54218775, \"2/815\"],\n[-37.87580405, 175.5118973167, \"126\"],\n[-37.86917495, 175.5157794167, \"1/208\"],\n[-37.8082772667, 175.5455867333, \"1212\"],\n[-37.8096694667, 175.5450804833, \"1194\"],\n[-37.8787073667, 175.5083375, \"79\"],\n[-37.8763409333, 175.51340955, \"3/110\"],\n[-37.8080369167, 175.5450349667, \"1213\"],\n[-37.8093666167, 175.5451384833, \"1196\"],\n[-37.82245505, 175.5477440333, \"1047\"],\n[-37.8775422667, 175.5104792, \"2/103\"],\n[-37.8236463333, 175.5474525, \"1033\"],\n[-37.8814960667, 175.50479415, \"33\"],\n[-37.8245330667, 175.5471272667, \"1023\"],\n[-37.8403625, 175.5402170167, \"1/809\"],\n[-37.8678366667, 175.5195366167, \"254\"],\n[-37.8790242333, 175.5103504333, \"4/88\"],\n[-37.8218208, 175.5477814333, \"1049\"],\n[-37.8789700833, 175.5084782833, \"77/1\"],\n[-37.8704926333, 175.5137601667, \"188\"],\n[-37.87878565, 175.5106821667, \"3/88\"],\n[-37.871913, 175.5121444333, \"163\"],\n[-37.87933595, 175.5101975333, \"6/88\"],\n[-37.8748227833, 175.5121225, \"136\"],\n[-37.87947495, 175.5108712167, \"5/88\"],\n[-37.8753615167, 175.5127019833, \"132\"],\n[-37.8109761333, 175.5426080167, \"1181\"],\n[-37.8773393667, 175.5116264833, \"104\"],\n[-37.8370667667, 175.5458885833, \"879\"],\n[-37.8765559833, 175.5101962833, \"103\"],\n[-37.8253379167, 175.5472683, \"989\"],\n[-37.8793315833, 175.5086546167, \"77\"],\n[-37.8661602, 175.52487915, \"316\"],\n[-37.8776637333, 175.50929225, \"3/103\"],\n[-37.8092819667, 175.5456565833, \"2/1200\"],\n[-37.87534805, 175.5120049833, \"128\"],\n[-37.81204605, 175.5446423333, \"1169\"],\n[-37.8752625833, 175.5140794833, \"138\"],\n[-37.8766464333, 175.5134206667, \"4/110\"],\n[-37.8770230167, 175.5108992333, \"113\"],\n[-37.8406207, 175.5394538167, \"2/809\"],\n[-37.8770699833, 175.5116503167, \"106\"],\n[-37.8122078667, 175.5440270833, \"1165\"],\n[-37.87518925, 175.5112472667, \"129\"],\n[-37.8507412333, 175.5347013833, \"633\"],\n[-37.8759461833, 175.5110732833, \"117\"],\n[-37.8210931833, 175.5477387333, \"1055\"],\n[-37.87555235, 175.5111759667, \"127\"],\n[-37.8180744833, 175.5471867333, \"1092\"],\n[-37.8761638, 175.5118200833, \"120\"],\n[-37.8714445, 175.5188348833, \"2/208\"],\n[-37.8738533, 175.5117649, \"141\"],\n[-37.8083899, 175.5443001833, \"1209\"],\n[-37.8672468667, 175.5243000167, \"295\"],\n[-37.8395333333, 175.54262905, \"1/815\"],\n[-37.8093977833, 175.5456164833, \"1/1200\"],\n[-37.86778915, 175.5240579167, \"290\"],\n[-37.8151683, 175.5450954333, \"1133\"],\n[-37.8676913333, 175.5179285833, \"235\"],\n[-37.87977015, 175.5090145833, \"66\"],\n[-37.8785379333, 175.5043763, \"1/39\"],\n[-37.8764341333, 175.51407215, \"5/110\"],\n[-37.8740516, 175.5125295667, \"146\"],\n[-37.87401495, 175.47760185, \"16\"],\n[-37.8739528333, 175.4764705667, \"31\"],\n[-37.8739846167, 175.4768385667, \"33\"],\n[-37.8748585167, 175.4767720333, \"11\"],\n[-37.8736880333, 175.4772042167, \"37\"],\n[-37.8747492333, 175.4769483667, \"9\"],\n[-37.87503235, 175.4775088667, \"3\"],\n[-37.87489515, 175.4779213833, \"6\"],\n[-37.8748158667, 175.4774378, \"5\"],\n[-37.8750760667, 175.4779026, \"4\"],\n[-37.8746501167, 175.4773401167, \"7\"],\n[-37.8741460667, 175.4765724167, \"27\"],\n[-37.8739166167, 175.477184, \"35\"],\n[-37.8741975833, 175.47717515, \"23\"],\n[-37.8738094, 175.47760075, \"18\"],\n[-37.8749257833, 175.4765688833, \"13\"],\n[-37.8741402667, 175.4763856, \"29\"],\n[-37.8742506833, 175.4775977, \"14\"],\n[-37.8741505, 175.47681925, \"25\"],\n[-37.8747261167, 175.4778583667, \"8\"],\n[-37.8745555, 175.4777541667, \"10\"],\n[-37.8743982667, 175.4777232, \"12\"],\n[-37.87440735, 175.4772111833, \"21\"],\n[-37.8745958667, 175.47674675, \"19\"],\n[-37.8747628333, 175.47645865, \"15\"],\n[-37.8745715333, 175.4764928667, \"17\"],\n[-37.9116880667, 175.4711713833, \"3\"],\n[-37.9116604333, 175.4715114333, \"1\"],\n[-37.9118500667, 175.4714183, \"5\"],\n[-37.9122240667, 175.4713923, \"11\"],\n[-37.9118788833, 175.4710693167, \"7\"],\n[-37.9123174667, 175.4714676167, \"10\"],\n[-37.91227905, 175.4716167167, \"8\"],\n[-37.9117270667, 175.4718080667, \"2\"],\n[-37.91205, 175.4713633167, \"9\"],\n[-37.912161, 175.4717712, \"6\"],\n[-37.8859179333, 175.4582119667, \"1\"],\n[-37.8865885333, 175.4581321667, \"6A\"],\n[-37.88640305, 175.45807455, \"6\"],\n[-37.8860451667, 175.4578916833, \"4D\"],\n[-37.8863091167, 175.45790615, \"5\"],\n[-37.8861527, 175.4579021833, \"4A\"],\n[-37.8859058833, 175.4578780333, \"3\"],\n[-37.8861333333, 175.4577295167, \"4B\"],\n[-37.8859079333, 175.45834695, \"1A\"],\n[-37.8861985, 175.4582370667, \"7\"],\n[-37.8860742333, 175.4577260833, \"4C\"],\n[-37.8858135, 175.4580266333, \"2\"],\n[-37.8856798, 175.4576616167, \"3B\"],\n[-37.8857954333, 175.45773405, \"3A\"],\n[-37.8039755167, 175.3924264333, \"588\"],\n[-37.8024268333, 175.3891336167, \"555\"],\n[-37.80621375, 175.3790952, \"476A\"],\n[-37.8035793667, 175.3898201333, \"565\"],\n[-37.8041731333, 175.3674844, \"348\"],\n[-37.8035751833, 175.38904805, \"559\"],\n[-37.8013125167, 175.390194, \"569A\"],\n[-37.8039518333, 175.3880558333, \"550\"],\n[-37.7993036333, 175.3774681, \"447B\"],\n[-37.80163095, 175.39138825, \"577\"],\n[-37.8035939833, 175.3922268667, \"583\"],\n[-37.8034719167, 175.3864473333, \"535A\"],\n[-37.80612815, 175.3786574833, \"476C\"],\n[-37.80604375, 175.3653441833, \"320\"],\n[-37.8030940833, 175.3837668333, \"515\"],\n[-37.8017065167, 175.37966555, \"471\"],\n[-37.8032808167, 175.3824573833, \"496C\"],\n[-37.8023511333, 175.3675479833, \"361\"],\n[-37.8029974167, 175.38875325, \"553B\"],\n[-37.8013158, 175.3713298333, \"400\"],\n[-37.8069269167, 175.38494245, \"522A\"],\n[-37.8041518333, 175.3666512333, \"342\"],\n[-37.8035926, 175.3904181, \"569B\"],\n[-37.8022781333, 175.3782524667, \"450A\"],\n[-37.8010726667, 175.3710681167, \"395\"],\n[-37.80130715, 175.37668905, \"445\"],\n[-37.8010362667, 175.3712212667, \"399\"],\n[-37.8032196333, 175.36794415, \"358\"],\n[-37.80095265, 175.3727210333, \"409\"],\n[-37.8021615667, 175.3808655833, \"481B\"],\n[-37.80149165, 175.3733540167, \"418\"],\n[-37.8010327833, 175.3739455167, \"419\"],\n[-37.8037261333, 175.3859318333, \"530\"],\n[-37.8019531333, 175.36978095, \"382\"],\n[-37.8035854333, 175.3674296167, \"352\"],\n[-37.8035820667, 175.3681635, \"358B\"],\n[-37.8044532833, 175.3898220167, \"562A\"],\n[-37.8030543167, 175.3681729333, \"360\"],\n[-37.80023085, 175.3753558333, \"429B\"],\n[-37.8029366333, 175.3683441833, \"362\"],\n[-37.8046987, 175.38796795, \"548\"],\n[-37.80282455, 175.3684962, \"364\"],\n[-37.8016347167, 175.3738360667, \"420\"],\n[-37.8031052333, 175.3689649333, \"366\"],\n[-37.8026689, 175.3687241167, \"368\"],\n[-37.8025527333, 175.3689042333, \"370\"],\n[-37.8037381333, 175.3672040667, \"350B\"],\n[-37.8015959333, 175.37030475, \"384\"],\n[-37.8037429667, 175.3861631667, \"532\"],\n[-37.8056256, 175.3854121, \"522D\"],\n[-37.8074032833, 175.37890055, \"476D\"],\n[-37.8033627833, 175.3941313, \"601\"],\n[-37.8035645833, 175.3885777333, \"553A\"],\n[-37.8037265833, 175.3751864333, \"438\"],\n[-37.80393295, 175.3820938333, \"496B\"],\n[-37.8039961667, 175.3937407167, \"600\"],\n[-37.8016335, 175.3785741333, \"455B\"],\n[-37.8028840667, 175.3674978, \"357\"],\n[-37.8009541833, 175.3783785, \"455C\"],\n[-37.8061105, 175.3849712833, \"518\"],\n[-37.8041071, 175.3675672667, \"350A\"],\n[-37.8039732667, 175.3922047667, \"586\"],\n[-37.7970054833, 175.3806949833, \"475B\"],\n[-37.80154115, 175.3776200833, \"455A\"],\n[-37.7968634833, 175.3813122667, \"475C\"],\n[-37.8040230333, 175.3951761833, \"610\"],\n[-37.8004082667, 175.3807690333, \"475D\"],\n[-37.8020559, 175.3804593667, \"481A\"],\n[-37.8029026667, 175.3814678667, \"490\"],\n[-37.79877225, 175.3804673833, \"475A\"],\n[-37.8024990833, 175.3803016, \"478\"],\n[-37.80798195, 175.3848864167, \"522B\"],\n[-37.8032268667, 175.3820611833, \"496A\"],\n[-37.8039424333, 175.3900004667, \"566\"],\n[-37.8055885667, 175.3849529667, \"508\"],\n[-37.8039439833, 175.3902647333, \"568\"],\n[-37.8044673167, 175.3791537667, \"476B\"],\n[-37.8034701, 175.3861407833, \"531\"],\n[-37.80394855, 175.3905212833, \"570\"],\n[-37.8033434667, 175.3677650667, \"356\"],\n[-37.8039541167, 175.3897445, \"564\"],\n[-37.8044864833, 175.3894950833, \"562B\"],\n[-37.8023883667, 175.3792474, \"468\"],\n[-37.8037905667, 175.3868461833, \"538\"],\n[-37.80376065, 175.3848380167, \"516\"],\n[-37.8037788333, 175.3866195333, \"536\"],\n[-37.8013236667, 175.3867703833, \"535B\"],\n[-37.80376085, 175.3863884, \"534\"],\n[-37.8038128667, 175.38531645, \"524\"],\n[-37.8038359, 175.3875590667, \"546\"],\n[-37.8053126, 175.3651698, \"328\"],\n[-37.8038053833, 175.3870689, \"540\"],\n[-37.80589585, 175.3892222167, \"560\"],\n[-37.8035248667, 175.3876434167, \"545\"],\n[-37.8037198, 175.3949872, \"609\"],\n[-37.80381955, 175.38729175, \"542\"],\n[-37.8075598333, 175.3855587, \"522C\"],\n[-37.8039602167, 175.3917614333, \"582\"],\n[-37.80139505, 175.3727005667, \"414\"],\n[-37.8036690667, 175.3910398667, \"575\"],\n[-37.8039518833, 175.3910117167, \"574\"],\n[-37.80395405, 175.3914879333, \"580\"],\n[-37.8002672167, 175.3771908, \"447A\"],\n[-37.8039521833, 175.3907747167, \"572\"],\n[-37.8036402667, 175.3914460167, \"579\"],\n[-37.803952, 175.3912312333, \"578\"],\n[-37.8015263667, 175.3694650333, \"383\"],\n[-37.80400045, 175.3949203833, \"608\"],\n[-37.8039955333, 175.3935219167, \"598\"],\n[-37.8039738167, 175.3926465167, \"590\"],\n[-37.8034657167, 175.3675994, \"354\"],\n[-37.8039642833, 175.3919945, \"584\"],\n[-37.8039832667, 175.3932899167, \"596\"],\n[-37.7988289, 175.3754860833, \"429C\"],\n[-37.80369095, 175.3927310167, \"591\"],\n[-37.80233225, 175.3911433833, \"571\"],\n[-37.8039846667, 175.3930810833, \"594\"],\n[-37.8013068333, 175.37155805, \"402\"],\n[-37.80397775, 175.3928504167, \"592\"],\n[-37.8040068833, 175.3948069833, \"608A\"],\n[-37.8037325167, 175.3941101167, \"603\"],\n[-37.8021858833, 175.3685292167, \"369\"],\n[-37.8039958, 175.3939723167, \"602\"],\n[-37.8048039833, 175.3657911, \"336\"],\n[-37.80368355, 175.39362055, \"599\"],\n[-37.8039906833, 175.3668667833, \"344\"],\n[-37.8037408333, 175.39457725, \"607\"],\n[-37.8040022167, 175.3941957, \"604\"],\n[-37.80357305, 175.3683457167, \"358A\"],\n[-37.80400475, 175.3944321167, \"606\"],\n[-37.8037340667, 175.3943487167, \"605\"],\n[-37.8021156667, 175.3876556833, \"543\"],\n[-37.8012321, 175.3754553833, \"429A\"],\n[-37.8018551667, 175.3752511, \"430\"],\n[-37.8038677333, 175.3670357333, \"346\"],\n[-37.8037207167, 175.3857282667, \"528\"],\n[-37.89186715, 175.4802623167, \"40\"],\n[-37.8923304833, 175.4777135167, \"13\"],\n[-37.8929612833, 175.4756593333, \"1\"],\n[-37.8918642, 175.4800987833, \"38\"],\n[-37.8926700333, 175.4763031833, \"5\"],\n[-37.89194195, 175.47893395, \"28A\"],\n[-37.8924575833, 175.4767435167, \"9\"],\n[-37.8918917333, 175.47938125, \"32\"],\n[-37.8922351667, 175.4796796333, \"27\"],\n[-37.8916363333, 175.47957795, \"34A\"],\n[-37.8922002833, 175.48006775, \"29\"],\n[-37.8918962833, 175.4796114333, \"34\"],\n[-37.8923002333, 175.4783952667, \"21\"],\n[-37.89225505, 175.4790280833, \"25\"],\n[-37.8920123333, 175.4773945167, \"10\"],\n[-37.8919046167, 175.4791651833, \"30\"],\n[-37.89170475, 175.4789158833, \"28B\"],\n[-37.89166225, 175.4793545667, \"32A\"],\n[-37.8919418833, 175.4787236, \"26\"],\n[-37.89187395, 175.4798382833, \"36\"],\n[-37.89226945, 175.4788097167, \"23\"],\n[-37.8919833667, 175.47800955, \"14\"],\n[-37.88193185, 175.4866305833, \"1\"],\n[-37.8812478, 175.4878395, \"20\"],\n[-37.8819307, 175.48686775, \"3\"],\n[-37.8823734333, 175.4877629833, \"8B\"],\n[-37.8821842833, 175.48700355, \"4\"],\n[-37.8818976833, 175.4871309333, \"5\"],\n[-37.8821982333, 175.4867615167, \"2\"],\n[-37.8821772, 175.48722635, \"6\"],\n[-37.8809340667, 175.4875154, \"24\"],\n[-37.8815621333, 175.4870228333, \"11B\"],\n[-37.88080855, 175.4873367167, \"26\"],\n[-37.8810877167, 175.4876951, \"22\"],\n[-37.8806647833, 175.4872178, \"25\"],\n[-37.8815580667, 175.4875735333, \"9\"],\n[-37.88050695, 175.48709835, \"23\"],\n[-37.8823652833, 175.48799725, \"10B\"],\n[-37.8817490333, 175.4878626667, \"12\"],\n[-37.88214405, 175.4874454833, \"8\"],\n[-37.8816923833, 175.4878766167, \"14\"],\n[-37.8823520667, 175.4875880667, \"8A\"],\n[-37.8814424167, 175.48790665, \"16\"],\n[-37.8819485333, 175.48774405, \"10\"],\n[-37.8813723833, 175.4878899, \"18\"],\n[-37.8822010167, 175.4879131167, \"10A\"],\n[-37.8825377667, 175.48796795, \"8C\"],\n[-37.8808839, 175.4866251333, \"17\"],\n[-37.8807889, 175.4868384833, \"19\"],\n[-37.8806788833, 175.4869695333, \"21\"],\n[-37.8818035, 175.48743315, \"7\"],\n[-37.8810684667, 175.4871791, \"13\"],\n[-37.8809503667, 175.4869225, \"15\"],\n[-37.8812931167, 175.4874573167, \"11\"],\n[-37.8812351167, 175.4869114833, \"13A\"],\n[-37.8814375667, 175.4871747, \"11A\"],\n[-37.8813577333, 175.4869692333, \"13B\"],\n[-37.9359592833, 175.5576784333, \"1/21\"],\n[-37.9380472167, 175.5564234833, \"20\"],\n[-37.9388147833, 175.5583222333, \"2/21\"],\n[-37.9370746333, 175.55679275, \"21\"],\n[-37.9396807833, 175.5590516333, \"4/21\"],\n[-37.9321653667, 175.5586632333, \"15\"],\n[-37.93777925, 175.5607732333, \"3/21\"],\n[-37.93261665, 175.5577030167, \"22\"],\n[-37.9350610667, 175.5544772, \"18\"],\n[-37.9324680333, 175.55825425, \"17\"],\n[-37.93286725, 175.5580919667, \"19\"],\n[-37.9655341, 175.48344125, \"228\"],\n[-37.9662557833, 175.4823937167, \"238\"],\n[-37.9551121167, 175.4819800167, \"74\"],\n[-37.97145005, 175.4810869667, \"298\"],\n[-37.96511045, 175.4842071167, \"226\"],\n[-37.9597843, 175.4837558833, \"1/144\"],\n[-37.9542875333, 175.4787352833, \"46\"],\n[-37.96095, 175.4848569, \"156\"],\n[-37.9608395667, 175.4825922167, \"144\"],\n[-37.9665397, 175.4859805667, \"227\"],\n[-37.9546337667, 175.4780326167, \"44\"],\n[-37.9662588833, 175.48466365, \"229\"],\n[-37.9774811333, 175.4827824167, \"367\"],\n[-37.9532187667, 175.4778035833, \"25\"],\n[-37.9779600167, 175.4820124333, \"380\"],\n[-37.96688945, 175.4825700833, \"244\"],\n[-37.9724996333, 175.4816797, \"317\"],\n[-37.9551293333, 175.4882803333, \"2/81\"],\n[-37.9544212167, 175.4829611333, \"79\"],\n[-37.9552378333, 175.4835594, \"3/81\"],\n[-37.9551674833, 175.4873534333, \"1/81\"],\n[-37.8177678333, 175.36481165, \"23\"],\n[-37.8176856667, 175.36528815, \"21\"],\n[-37.81793065, 175.36431665, \"24\"],\n[-37.8188466, 175.3655251167, \"13\"],\n[-37.8183913167, 175.3654774833, \"14\"],\n[-37.8186506667, 175.3663757833, \"8\"],\n[-37.9091586833, 175.4806199667, \"8\"],\n[-37.9097846667, 175.4804238833, \"5\"],\n[-37.9095970833, 175.4803937333, \"6\"],\n[-37.90949585, 175.48088805, \"2\"],\n[-37.9097125667, 175.4807851667, \"3\"],\n[-37.9090767333, 175.4810419167, \"1\"],\n[-37.9098378, 175.4806015167, \"4\"],\n[-37.9094125333, 175.48047265, \"7\"],\n[-37.8987957167, 175.4867648167, \"4\"],\n[-37.8993040833, 175.48608325, \"13A\"],\n[-37.8986618667, 175.4863548, \"3\"],\n[-37.8990157167, 175.4866799667, \"8\"],\n[-37.8988486167, 175.48625515, \"11\"],\n[-37.89940275, 175.48652375, \"12\"],\n[-37.8990940333, 175.4857358333, \"15\"],\n[-37.8994477, 175.4860294167, \"13\"],\n[-37.8984905333, 175.4864897167, \"1\"],\n[-37.8910458667, 175.4604486167, \"4A\"],\n[-37.8909012833, 175.4603024667, \"1\"],\n[-37.8912700667, 175.4604892667, \"4B\"],\n[-37.8909202167, 175.4608893333, \"3C\"],\n[-37.8912779333, 175.4605982333, \"4C\"],\n[-37.8909172, 175.4610096, \"3D\"],\n[-37.8910309833, 175.46058295, \"4D\"],\n[-37.8909275167, 175.4607770667, \"3B\"],\n[-37.8908830667, 175.46040695, \"2\"],\n[-37.8907294667, 175.4603703, \"2B\"],\n[-37.8909360167, 175.4606728333, \"3A\"],\n[-37.8910318, 175.46011345, \"5\"],\n[-37.9038995167, 175.4740167833, \"1\"],\n[-37.9040010833, 175.4744881, \"3\"],\n[-37.9039700667, 175.4742456667, \"2\"],\n[-37.9041160333, 175.4738995667, \"9\"],\n[-37.9042205, 175.4746609667, \"5\"],\n[-37.90408835, 175.4746666167, \"4\"],\n[-37.9043300333, 175.4745611833, \"6\"],\n[-37.9042862667, 175.47435, \"7\"],\n[-37.9041895, 175.4741327333, \"8\"],\n[-37.8412972333, 175.51498905, \"2/370\"],\n[-37.85380915, 175.5048969, \"197\"],\n[-37.8393792167, 175.5185004833, \"4/370\"],\n[-37.8356004, 175.5155473833, \"1/446\"],\n[-37.8516975167, 175.5088596333, \"230\"],\n[-37.83493705, 175.5162245, \"2/446\"],\n[-37.8630868833, 175.5013258667, \"77\"],\n[-37.8341336333, 175.5147212667, \"454\"],\n[-37.8317585167, 175.511789, \"495\"],\n[-37.8378645667, 175.51179665, \"422\"],\n[-37.8482979667, 175.5057717167, \"2/277\"],\n[-37.8384529, 175.5107566, \"1/413\"],\n[-37.8543168833, 175.5057733167, \"200\"],\n[-37.8382859, 175.51004915, \"2/413\"],\n[-37.8302591667, 175.51099635, \"520\"],\n[-37.8430264833, 175.5117441667, \"361\"],\n[-37.8569890167, 175.50014995, \"145\"],\n[-37.8546612333, 175.51046255, \"4/210\"],\n[-37.8291138167, 175.5100652833, \"537\"],\n[-37.8644712, 175.5010749167, \"57\"],\n[-37.8537165667, 175.5061015667, \"1/210\"],\n[-37.8561494333, 175.5002218833, \"151\"],\n[-37.8531273167, 175.50591205, \"207\"],\n[-37.8661337333, 175.50044185, \"39\"],\n[-37.8572501333, 175.50068855, \"142\"],\n[-37.86749845, 175.4999983667, \"31\"],\n[-37.8311046167, 175.5124198667, \"504\"],\n[-37.8610681167, 175.5009140667, \"99\"],\n[-37.86009745, 175.5013829333, \"110\"],\n[-37.8635882333, 175.5025296333, \"2/72\"],\n[-37.8381455333, 175.5095623833, \"415\"],\n[-37.86345265, 175.5022295333, \"1/72\"],\n[-37.8535591333, 175.5063429, \"3/210\"],\n[-37.8428438667, 175.5239404333, \"8/370\"],\n[-37.8425296667, 175.5133880833, \"1/370\"],\n[-37.8415704667, 175.5230515333, \"7/370\"],\n[-37.8607450167, 175.5008543833, \"101\"],\n[-37.8402798167, 175.5220041667, \"6/370\"],\n[-37.8540611833, 175.5052768833, \"198\"],\n[-37.8457524833, 175.5094666, \"314\"],\n[-37.8379597667, 175.51065885, \"417\"],\n[-37.83947605, 175.5206863, \"5/370\"],\n[-37.8301105, 175.5103759, \"521\"],\n[-37.84097385, 175.5217665167, \"9/370\"],\n[-37.8408829167, 175.5157633333, \"3/370\"],\n[-37.8540650833, 175.5067036667, \"2/210\"],\n[-37.8541100167, 175.5042267333, \"191\"],\n[-37.8426532167, 175.5117339667, \"365\"],\n[-37.8483104167, 175.50609355, \"1/277\"],\n[-37.8610126167, 175.5014974, \"98\"],\n[-37.8409556, 175.5110372667, \"383\"],\n[-37.8563855167, 175.4999012333, \"149\"],\n[-37.8591389167, 175.5012515167, \"118\"],\n[-37.8622922667, 175.5017081, \"86\"],\n[-37.8279955333, 175.5107170333, \"548\"],\n[-37.8293633667, 175.5100288667, \"535\"],\n[-37.87412315, 175.36871325, \"20\"],\n[-37.8741985167, 175.3678984333, \"25\"],\n[-37.8744552833, 175.36791385, \"23\"],\n[-37.82354435, 175.3645601333, \"31\"],\n[-37.81426245, 175.3697857667, \"140B\"],\n[-37.8249820167, 175.36436295, \"18A\"],\n[-37.8247545333, 175.3644630667, \"18B\"],\n[-37.8196718, 175.3690864167, \"74B\"],\n[-37.8141992167, 175.3685047667, \"140D\"],\n[-37.8202965167, 175.3688420833, \"74C\"],\n[-37.8180317333, 175.3670563833, \"97\"],\n[-37.8166606, 175.3680389833, \"116\"],\n[-37.81378495, 175.3674693667, \"143\"],\n[-37.8140329833, 175.36750105, \"141\"],\n[-37.8230129333, 175.36477085, \"33\"],\n[-37.8181065333, 175.367528, \"96A\"],\n[-37.822355, 175.3651528333, \"43\"],\n[-37.8207430167, 175.37001735, \"72\"],\n[-37.8175852333, 175.36768535, \"104\"],\n[-37.8240652167, 175.3643383167, \"25\"],\n[-37.8208649167, 175.3708165167, \"72C\"],\n[-37.8158759333, 175.3699935167, \"124A\"],\n[-37.81611735, 175.3726213833, \"124D\"],\n[-37.8141102833, 175.3710208167, \"140E\"],\n[-37.8158552833, 175.3725671, \"126C\"],\n[-37.8172870667, 175.3672759333, \"105\"],\n[-37.8149043167, 175.3680942, \"126A\"],\n[-37.8156860667, 175.3714600667, \"126B\"],\n[-37.82234555, 175.3676551167, \"58C\"],\n[-37.8229201, 175.3663306333, \"42B\"],\n[-37.8217244667, 175.36615365, \"56\"],\n[-37.82168065, 175.3671740333, \"58A\"],\n[-37.8226533, 175.3656960667, \"42A\"],\n[-37.81969495, 175.3671185, \"74A\"],\n[-37.8139910833, 175.3680670333, \"142\"],\n[-37.8222810667, 175.36812165, \"58B\"],\n[-37.8160942833, 175.3717498333, \"124C\"],\n[-37.8210101167, 175.3697359333, \"72D\"],\n[-37.8199424667, 175.3700336667, \"72A\"],\n[-37.8162840333, 175.36763115, \"115\"],\n[-37.8169082, 175.3674243667, \"109\"],\n[-37.8231334167, 175.3666729833, \"42C\"],\n[-37.8211535833, 175.3664307, \"66\"],\n[-37.8159958, 175.3710006, \"124B\"],\n[-37.8230926333, 175.3654223667, \"40\"],\n[-37.8142580167, 175.3692795333, \"140C\"],\n[-37.8184104667, 175.3677529833, \"96B\"],\n[-37.8232137667, 175.3664941667, \"42D\"],\n[-37.8238653167, 175.3649438333, \"30\"],\n[-37.8210668833, 175.3659746167, \"59\"],\n[-37.8204322833, 175.3668945833, \"70\"],\n[-37.8141750667, 175.3703629167, \"140A\"],\n[-37.8203761167, 175.3663, \"69\"],\n[-37.8190875, 175.3666263, \"83\"],\n[-37.8206303833, 175.37079745, \"72B\"],\n[-37.8159921167, 175.36829495, \"120\"],\n[-37.8702900833, 175.4823576, \"129\"],\n[-37.8691575833, 175.4875552167, \"174\"],\n[-37.8752810167, 175.4785701333, \"29\"],\n[-37.8739353333, 175.4787035333, \"71\"],\n[-37.8694349833, 175.4853619333, \"150\"],\n[-37.86819925, 175.4904272667, \"203\"],\n[-37.8739757333, 175.4791716, \"36\"],\n[-37.8743164833, 175.47849545, \"69\"],\n[-37.8764327833, 175.4784211167, \"18\"],\n[-37.8772371, 175.47724965, \"11\"],\n[-37.8711581167, 175.4812798, \"99\"],\n[-37.8774015333, 175.4771253167, \"9\"],\n[-37.8747281, 175.4786202833, \"35\"],\n[-37.8769075, 175.47753225, \"15\"],\n[-37.8764091, 175.4779333667, \"21\"],\n[-37.8751066833, 175.47903655, \"28\"],\n[-37.8755713333, 175.4789569333, \"24\"],\n[-37.8767426833, 175.4776735667, \"17\"],\n[-37.87754365, 175.4769945167, \"7\"],\n[-37.8749176, 175.4790524, \"30\"],\n[-37.86789545, 175.4922832833, \"215\"],\n[-37.8762449333, 175.4780463833, \"23\"],\n[-37.8746104333, 175.4786632333, \"37\"],\n[-37.87535045, 175.4789887, \"26\"],\n[-37.8690190833, 175.4889257667, \"188\"],\n[-37.8766569, 175.4782477833, \"16\"],\n[-37.8777791667, 175.4743660833, \"3F\"],\n[-37.8760992833, 175.4781375667, \"25\"],\n[-37.8735414833, 175.4792975667, \"40\"],\n[-37.8749123833, 175.4786099333, \"33\"],\n[-37.8778868667, 175.4755309333, \"3B\"],\n[-37.8757382667, 175.4789290833, \"22\"],\n[-37.8690839, 175.4882992333, \"182\"],\n[-37.8778719167, 175.4752090167, \"3C\"],\n[-37.8692440333, 175.487114, \"168\"],\n[-37.87786735, 175.4759980333, \"3A\"],\n[-37.8750885833, 175.4785857167, \"31\"],\n[-37.8778593, 175.4767417, \"1\"],\n[-37.8770541, 175.4774016167, \"13\"],\n[-37.8693264333, 175.4864033167, \"160\"],\n[-37.8776720667, 175.47687455, \"5\"],\n[-37.87418305, 175.4791450333, \"34\"],\n[-37.8772782167, 175.4778079333, \"12\"],\n[-37.8694907, 175.48387845, \"139\"],\n[-37.8737682833, 175.4792036833, \"38\"],\n[-37.8780202167, 175.4743749, \"3E\"],\n[-37.87657365, 175.4778033833, \"19\"],\n[-37.8769227, 175.4780192833, \"14\"],\n[-37.8778372, 175.4747625833, \"3D\"],\n[-37.9058634167, 175.4818391833, \"11\"],\n[-37.90651765, 175.4816564, \"7\"],\n[-37.90600205, 175.4817922833, \"10\"],\n[-37.9065382667, 175.4819836667, \"6\"],\n[-37.9064549667, 175.48231495, \"4\"],\n[-37.9065751, 175.4821868667, \"5\"],\n[-37.9062895, 175.4822382667, \"3\"],\n[-37.9061909, 175.4817668, \"9\"],\n[-37.9058636333, 175.4821877667, \"1\"],\n[-37.90609375, 175.48208955, \"2\"],\n[-37.9063477667, 175.4818540833, \"8\"],\n[-37.9028285, 175.4830620667, \"10\"],\n[-37.9033403333, 175.4828242833, \"4\"],\n[-37.9026557833, 175.4836036833, \"11\"],\n[-37.9026651667, 175.4831392167, \"12\"],\n[-37.9024793, 175.4836898833, \"13\"],\n[-37.9024864833, 175.4832286333, \"14\"],\n[-37.9022771167, 175.4837931167, \"15\"],\n[-37.9023275333, 175.48331475, \"16\"],\n[-37.9021481167, 175.4833864333, \"18\"],\n[-37.9034164167, 175.48324155, \"3\"],\n[-37.9020819667, 175.4837508833, \"17\"],\n[-37.9032251, 175.4833323833, \"5\"],\n[-37.9031833833, 175.4829241667, \"6\"],\n[-37.9030398667, 175.4834288667, \"7\"],\n[-37.9030060333, 175.4827531167, \"8A\"],\n[-37.90303195, 175.4830065, \"8\"],\n[-37.90285465, 175.48352545, \"9\"],\n[-37.9030180333, 175.4855010667, \"50\"],\n[-37.90297345, 175.4847805333, \"52\"],\n[-37.90302055, 175.4852138333, \"50A\"],\n[-37.9006098833, 175.4854521833, \"33\"],\n[-37.9029307667, 175.485268, \"48\"],\n[-37.9014686667, 175.48551475, \"36\"],\n[-37.9017719833, 175.4853557833, \"40\"],\n[-37.9016177667, 175.4854306833, \"38\"],\n[-37.9023493667, 175.48468465, \"47\"],\n[-37.9027064667, 175.4842364667, \"51A\"],\n[-37.9019256667, 175.4852900333, \"42\"],\n[-37.9016388667, 175.4849819167, \"41\"],\n[-37.9014681667, 175.485061, \"39\"],\n[-37.9033233167, 175.48461845, \"56\"],\n[-37.9037893833, 175.4839462667, \"63\"],\n[-37.9031498833, 175.4846976667, \"54\"],\n[-37.9026018, 175.48495995, \"44\"],\n[-37.9030769833, 175.4842938, \"55\"],\n[-37.9027864167, 175.4848613, \"46\"],\n[-37.9037959667, 175.4843982667, \"62\"],\n[-37.90251565, 175.4845560333, \"49\"],\n[-37.9039613167, 175.4843144, \"64\"],\n[-37.9027125667, 175.4844694167, \"51\"],\n[-37.9034637167, 175.4841226, \"59\"],\n[-37.9028946833, 175.4843869, \"53\"],\n[-37.9036489333, 175.4840298833, \"61\"],\n[-37.90327315, 175.4842082833, \"57\"],\n[-37.90046945, 175.4855198667, \"29\"],\n[-37.9010344833, 175.4856939833, \"34\"],\n[-37.9013283667, 175.4851346333, \"37\"],\n[-37.9017672167, 175.4849186, \"43\"],\n[-37.8611247, 175.4102509167, \"175\"],\n[-37.8499004667, 175.4102191667, \"47\"],\n[-37.86802365, 175.4100161, \"251\"],\n[-37.8527479167, 175.4097375667, \"82\"],\n[-37.84663775, 175.41026955, \"13\"],\n[-37.8556193, 175.4102112167, \"113\"],\n[-37.8623374833, 175.4096021667, \"188\"],\n[-37.8570313833, 175.4097304667, \"128\"],\n[-37.8508672833, 175.4096890333, \"58\"],\n[-37.8633539667, 175.4102016833, \"197\"],\n[-37.8624295167, 175.4113314833, \"187\"],\n[-37.8643999167, 175.4142555167, \"211A\"],\n[-37.8674836667, 175.4109264333, \"245A\"],\n[-37.8645949333, 175.4142231333, \"211B\"],\n[-37.863722, 175.4116543667, \"203\"],\n[-37.8652689, 175.4102405667, \"217\"],\n[-37.8674028167, 175.4119272333, \"245B\"],\n[-37.8688986, 175.4071594333, \"283\"],\n[-37.8626189667, 175.4102669333, \"191\"],\n[-37.86890705, 175.4066821333, \"285\"],\n[-37.8638613833, 175.4102196333, \"205\"],\n[-37.8520415333, 175.4096858833, \"74\"],\n[-37.85366185, 175.4103740333, \"91\"],\n[-37.8541498167, 175.4102157, \"99\"],\n[-37.8481882167, 175.4096517333, \"32\"],\n[-37.8998180833, 175.4671585833, \"1\"],\n[-37.8994876333, 175.46689395, \"4\"],\n[-37.8995568833, 175.4671854, \"2\"],\n[-37.89982245, 175.4669492, \"3\"],\n[-37.8997823167, 175.4667780333, \"5\"],\n[-37.8993499, 175.4665456333, \"6\"],\n[-37.8994541, 175.4664421167, \"8\"],\n[-37.8996415, 175.4667272333, \"7\"],\n[-37.8916384833, 175.4630697, \"40\"],\n[-37.8913306, 175.4610441333, \"24\"],\n[-37.89161595, 175.4716677667, \"133\"],\n[-37.8914569167, 175.4754211833, \"149\"],\n[-37.89160835, 175.4718250167, \"135\"],\n[-37.89234715, 175.4585814667, \"9\"],\n[-37.8915247667, 175.4732450833, \"137\"],\n[-37.8916493667, 175.4628682, \"36\"],\n[-37.89127365, 175.4608001667, \"1/20-3/20\"],\n[-37.8922152167, 175.4605929667, \"17\"],\n[-37.89145415, 175.4607833333, \"4/20-7/20\"],\n[-37.8917261333, 175.4624021, \"32\"],\n[-37.8919335167, 175.45821125, \"4C\"],\n[-37.8922107833, 175.4609609833, \"21\"],\n[-37.8919453667, 175.4579664333, \"4B\"],\n[-37.8915077, 175.4658864333, \"66\"],\n[-37.8919506333, 175.4577320333, \"4A\"],\n[-37.8916613167, 175.4626608667, \"34\"],\n[-37.8919576833, 175.4575137, \"2C\"],\n[-37.89126035, 175.4701749167, \"92\"],\n[-37.8922950667, 175.4588386, \"11\"],\n[-37.8921412333, 175.4623956833, \"31\"],\n[-37.8918877167, 175.4588751167, \"10\"],\n[-37.8912372, 175.4708680333, \"98\"],\n[-37.8920993333, 175.4636887167, \"43\"],\n[-37.89223965, 175.4603451, \"15\"],\n[-37.8920777333, 175.4638900667, \"43A\"],\n[-37.89235135, 175.4580704, \"5\"],\n[-37.8916213667, 175.4634083333, \"44\"],\n[-37.8913535, 175.4751499333, \"147\"],\n[-37.8911645667, 175.4634580167, \"46\"],\n[-37.89154565, 175.460355, \"16A\"],\n[-37.8916286667, 175.4636265667, \"48\"],\n[-37.8914890667, 175.4661530833, \"68\"],\n[-37.8916243333, 175.46377045, \"50A\"],\n[-37.8921945667, 175.4613726833, \"25\"],\n[-37.8916212333, 175.4638566667, \"50\"],\n[-37.8917585833, 175.4617233333, \"28\"],\n[-37.8916201, 175.4640203, \"52\"],\n[-37.89219925, 175.46119465, \"23\"],\n[-37.891597, 175.46425765, \"54\"],\n[-37.8918353, 175.46017655, \"14\"],\n[-37.8919748667, 175.4649644667, \"53\"],\n[-37.8917905667, 175.4608055333, \"22\"],\n[-37.89131955, 175.4756406, \"153\"],\n[-37.8923704333, 175.4576240833, \"3\"],\n[-37.8905614333, 175.4758690167, \"154\"],\n[-37.8916816167, 175.4707503, \"97\"],\n[-37.8913018667, 175.4759012333, \"155\"],\n[-37.89253265, 175.4612154167, \"23B\"],\n[-37.8909466833, 175.4760167333, \"156\"],\n[-37.8920906167, 175.4634921, \"41\"],\n[-37.8912217, 175.4774480833, \"165\"],\n[-37.8921237833, 175.46277795, \"35\"],\n[-37.8908697333, 175.477294, \"162\"],\n[-37.8920948667, 175.4632665833, \"39\"],\n[-37.8912003333, 175.47770905, \"167\"],\n[-37.8914414833, 175.4678414, \"76\"],\n[-37.8908453833, 175.4777106667, \"168\"],\n[-37.8912160667, 175.4629456833, \"38\"],\n[-37.8911874, 175.4779408167, \"169\"],\n[-37.8919186333, 175.4584176833, \"6\"],\n[-37.89118215, 175.4781761167, \"171\"],\n[-37.8916446833, 175.4632172667, \"42\"],\n[-37.8911654333, 175.4784059833, \"173\"],\n[-37.8918217333, 175.4603906, \"16\"],\n[-37.8911525333, 175.47862755, \"175\"],\n[-37.8924511333, 175.4606313, \"17B\"],\n[-37.8907954, 175.4784089, \"176\"],\n[-37.8915381333, 175.46524225, \"58\"],\n[-37.8911355667, 175.478864, \"177\"],\n[-37.892167, 175.4617876833, \"29\"],\n[-37.8911190667, 175.4790793167, \"179\"],\n[-37.89148925, 175.47413605, \"141\"],\n[-37.891102, 175.4793148833, \"181\"],\n[-37.89174025, 175.4622023, \"30\"],\n[-37.8910930167, 175.4795416333, \"183\"],\n[-37.8923048, 175.4605938667, \"17A\"],\n[-37.8907614, 175.4793265833, \"184\"],\n[-37.8919164833, 175.45868485, \"8\"],\n[-37.89108015, 175.4797723833, \"185\"],\n[-37.8905659667, 175.4757830667, \"152\"],\n[-37.8910595667, 175.4800169167, \"187\"],\n[-37.8923372333, 175.4582542, \"7\"],\n[-37.8917307333, 175.46930505, \"83\"],\n[-37.8909663333, 175.4756883833, \"150\"],\n[-37.8909111167, 175.47660485, \"160\"],\n[-37.8916525667, 175.47116215, \"129\"],\n[-37.8909366167, 175.4762755333, \"158\"],\n[-37.8911956, 175.4715676333, \"104\"],\n[-37.8915429, 175.4650714667, \"56\"],\n[-37.8916859833, 175.45866365, \"8A\"],\n[-37.8919523, 175.4651683833, \"55\"],\n[-37.8915248667, 175.4654342167, \"60\"],\n[-37.8917482, 175.4613051333, \"26\"],\n[-37.89101255, 175.4753879, \"148\"],\n[-37.8925110833, 175.4608349833, \"19\"],\n[-37.89179885, 175.4605882333, \"18\"],\n[-37.8918582667, 175.4680386, \"79\"],\n[-37.8921763167, 175.461584, \"27\"],\n[-37.8914747833, 175.4665362167, \"70\"],\n[-37.8923905833, 175.4611956167, \"23A\"],\n[-37.8919975333, 175.4651655, \"55A-55E\"],\n[-37.8920026167, 175.45729865, \"2B\"],\n[-37.8920162667, 175.4570673, \"2A\"],\n[-37.8916384333, 175.4713722, \"131\"],\n[-37.8912288333, 175.4711408333, \"102\"],\n[-37.8915248167, 175.4656389833, \"62\"],\n[-37.8917611667, 175.46114545, \"26A\"],\n[-37.8921091, 175.4630288167, \"37\"],\n[-37.8209873167, 175.3760669333, \"191C\"],\n[-37.8249917, 175.3656036667, \"91A\"],\n[-37.8190006167, 175.3860204167, \"254B\"],\n[-37.818206, 175.38533, \"254C\"],\n[-37.8061591667, 175.3983436, \"462\"],\n[-37.8056489333, 175.3937574167, \"423\"],\n[-37.79713005, 175.40084965, \"545\"],\n[-37.79324805, 175.4048036667, \"601\"],\n[-37.8186617833, 175.3815952, \"248A\"],\n[-37.8090422833, 175.3906621333, \"383B\"],\n[-37.8183971667, 175.3818555667, \"248B\"],\n[-37.8027335833, 175.3963706167, \"471\"],\n[-37.8167087, 175.3801758333, \"267D\"],\n[-37.7975265333, 175.4015703333, \"548\"],\n[-37.8190651167, 175.3786646333, \"229\"],\n[-37.7903981, 175.4069922833, \"639B\"],\n[-37.8253436333, 175.3662536, \"91B\"],\n[-37.8085089, 175.3916918167, \"406\"],\n[-37.82507135, 175.36734685, \"107A\"],\n[-37.8243141667, 175.3677190167, \"107B\"],\n[-37.8240930167, 175.3684544333, \"107C\"],\n[-37.82428045, 175.3692349, \"107D\"],\n[-37.79320855, 175.4054745333, \"606\"],\n[-37.8210957833, 175.3758549, \"191A\"],\n[-37.8220672667, 175.3711244167, \"155B\"],\n[-37.7905658333, 175.40715725, \"639A\"],\n[-37.8219987167, 175.37182365, \"155C\"],\n[-37.79282425, 175.4050586, \"605\"],\n[-37.8217745833, 175.37222335, \"155D\"],\n[-37.80515685, 175.3938695, \"437\"],\n[-37.8076407333, 175.3900775, \"397\"],\n[-37.8237106, 175.3712966333, \"131\"],\n[-37.8015971, 175.3955181667, \"479\"],\n[-37.8227929, 175.3730921167, \"157\"],\n[-37.7961391167, 175.4003037, \"549\"],\n[-37.8233609833, 175.37310895, \"152\"],\n[-37.8219492667, 175.3745148667, \"167\"],\n[-37.8247903833, 175.3685883333, \"109\"],\n[-37.8248283, 175.3705193167, \"126\"],\n[-37.8109710333, 175.3856751167, \"341\"],\n[-37.82605485, 175.3651553667, \"80\"],\n[-37.8107359, 175.3894516833, \"368\"],\n[-37.8120085667, 175.3845064, \"321\"],\n[-37.8128636833, 175.38379265, \"310\"],\n[-37.81208235, 175.3843228, \"319\"],\n[-37.8046513333, 175.3956590833, \"452\"],\n[-37.8144067333, 175.3816008667, \"275\"],\n[-37.8102773, 175.3874213, \"345B\"],\n[-37.8144369, 175.38227185, \"280\"],\n[-37.8164895333, 175.3782892333, \"267C\"],\n[-37.8182382167, 175.3800216667, \"240\"],\n[-37.8173681833, 175.3806125, \"248C\"],\n[-37.8181792333, 175.3832736333, \"254A\"],\n[-37.8095723, 175.3899326833, \"383A\"],\n[-37.81761125, 175.3835261667, \"254D\"],\n[-37.8028805167, 175.3962506667, \"469\"],\n[-37.8185789, 175.3845444, \"254\"],\n[-37.8163547167, 175.3812859833, \"258\"],\n[-37.7930282167, 175.40572125, \"608\"],\n[-37.821138, 175.38039485, \"216D\"],\n[-37.81606355, 175.3793317167, \"267B\"],\n[-37.81934515, 175.3785087833, \"225\"],\n[-37.8050155667, 175.3943832667, \"443A\"],\n[-37.81861015, 175.379036, \"235A\"],\n[-37.8121421833, 175.3842917833, \"317\"],\n[-37.7913696833, 175.4063793, \"623\"],\n[-37.8205651167, 175.3768472333, \"201\"],\n[-37.8205108, 175.37795785, \"212\"],\n[-37.8231057, 175.3725295333, \"155A\"],\n[-37.8046846167, 175.3939956333, \"443B\"],\n[-37.7964915667, 175.40187745, \"559\"],\n[-37.80485815, 175.39453205, \"443C\"],\n[-37.8060723667, 175.3939798167, \"428\"],\n[-37.8047227, 175.3946713167, \"445\"],\n[-37.8048381667, 175.3949814667, \"448\"],\n[-37.8045786833, 175.3947850833, \"449\"],\n[-37.7981357, 175.3900883333, \"461B\"],\n[-37.8099047, 175.39060605, \"388\"],\n[-37.8043014333, 175.3950148, \"453\"],\n[-37.8043075833, 175.3954701167, \"456\"],\n[-37.79168895, 175.4068591833, \"626\"],\n[-37.8021525833, 175.39678165, \"481\"],\n[-37.8034197833, 175.39575665, \"463\"],\n[-37.8032362167, 175.3959217833, \"467\"],\n[-37.80216475, 175.3974925167, \"484\"],\n[-37.8203596833, 175.3755115833, \"191B\"],\n[-37.7993380667, 175.3999904667, \"522\"],\n[-37.8085763833, 175.3912222, \"391\"],\n[-37.7981168833, 175.4004628, \"533\"],\n[-37.7969743333, 175.4013795333, \"553\"],\n[-37.7970796333, 175.4021227667, \"554\"],\n[-37.7943688833, 175.4045175833, \"590\"],\n[-37.8073738833, 175.39222815, \"419\"],\n[-37.7955603667, 175.4027828667, \"569\"],\n[-37.8102608, 175.3842148333, \"345A\"],\n[-37.7958160167, 175.4031990833, \"570\"],\n[-37.7924329333, 175.4084768167, \"630\"],\n[-37.7956436833, 175.4033528333, \"572\"],\n[-37.7960458333, 175.4022678, \"563\"],\n[-37.8030297833, 175.3966600167, \"472\"],\n[-37.7963875, 175.4000470667, \"547\"],\n[-37.8052467, 175.39419005, \"439\"],\n[-37.8103165333, 175.38810555, \"345C\"],\n[-37.7987565833, 175.3998819167, \"531A\"],\n[-37.7982830833, 175.40023825, \"531B\"],\n[-37.79918855, 175.4000808833, \"524\"],\n[-37.7944693667, 175.4027557167, \"577\"],\n[-37.7950937833, 175.4031431333, \"575\"],\n[-37.7927918333, 175.4046828167, \"603\"],\n[-37.7947576333, 175.4034387833, \"579\"],\n[-37.8117228167, 175.38764085, \"346B\"],\n[-37.7911496667, 175.40664935, \"629\"],\n[-37.81588535, 175.3807131667, \"267A\"],\n[-37.7912028, 175.40724595, \"634\"],\n[-37.7908960167, 175.4074944, \"638\"],\n[-37.8108278167, 175.3873807333, \"346A\"],\n[-37.8109458667, 175.3868272333, \"346\"],\n[-37.8861902333, 175.4912457, \"146\"],\n[-37.8868429667, 175.4923818, \"242\"],\n[-37.8863162333, 175.4914635667, \"150\"],\n[-37.8866134167, 175.4930928167, \"281\"],\n[-37.8857069333, 175.49160115, \"115\"],\n[-37.88701045, 175.4928565167, \"282\"],\n[-37.8865745167, 175.4918085333, \"196\"],\n[-37.8847997667, 175.49160355, \"1\"],\n[-37.88530765, 175.49159645, \"65\"],\n[-37.8858858, 175.4924325333, \"195\"],\n[-37.88625505, 175.4922030167, \"201\"],\n[-37.8858815167, 175.49117815, \"140\"],\n[-37.8870972167, 175.4937585167, \"370\"],\n[-37.8871422667, 175.4932273333, \"330\"],\n[-37.8867199333, 175.4935654, \"331\"],\n[-37.88711285, 175.4670386167, \"6\"],\n[-37.8867336333, 175.4670516667, \"3\"],\n[-37.8867436833, 175.4669519, \"1\"],\n[-37.8869533833, 175.4669386667, \"2\"],\n[-37.8869398667, 175.46704125, \"4\"],\n[-37.8871175, 175.46692505, \"5\"],\n[-37.9008030333, 175.4861118167, \"2\"],\n[-37.90075285, 175.4859038833, \"1\"],\n[-37.9009336167, 175.4865805833, \"4\"],\n[-37.9008800167, 175.4863431167, \"3\"],\n[-37.90100455, 175.4868142, \"5\"],\n[-37.9014235667, 175.4873154667, \"7B\"],\n[-37.9010728, 175.4870406333, \"6\"],\n[-37.9012685833, 175.4873578833, \"7A\"],\n[-37.9011401833, 175.4872876833, \"7\"],\n[-37.8158733667, 175.37666665, \"18A\"],\n[-37.8161566833, 175.3760580333, \"16\"],\n[-37.8161400333, 175.3766246833, \"18B\"],\n[-37.81564025, 175.3760005833, \"20\"],\n[-37.9147940667, 175.4781293167, \"2\"],\n[-37.91445275, 175.477954, \"5\"],\n[-37.91432165, 175.4780204333, \"7\"],\n[-37.9142254833, 175.4781627667, \"9\"],\n[-37.9142594833, 175.4783362167, \"11\"],\n[-37.9140805833, 175.4785308167, \"11A\"],\n[-37.9141151333, 175.47861905, \"10A\"],\n[-37.91436635, 175.4784698833, \"10\"],\n[-37.9144396667, 175.4784653667, \"8\"],\n[-37.9147089667, 175.47783895, \"1\"],\n[-37.9145894, 175.4778914333, \"3\"],\n[-37.9146597833, 175.4781811833, \"4\"],\n[-37.9145286, 175.4783673833, \"6\"],\n[-37.9030873667, 175.47210575, \"3\"],\n[-37.9032704833, 175.472054, \"5\"],\n[-37.9032275333, 175.4724681, \"4\"],\n[-37.9029155167, 175.4722868333, \"1\"],\n[-37.9029878333, 175.4725947167, \"2\"],\n[-37.9033794333, 175.4723403167, \"6\"],\n[-37.9033877, 175.4721768833, \"8\"],\n[-37.9026622333, 175.5342312833, \"2/60\"],\n[-37.8962902167, 175.5377237833, \"3/157\"],\n[-37.9056577833, 175.5300138, \"24\"],\n[-37.8978118667, 175.53526435, \"1/157\"],\n[-37.9036869333, 175.5324068, \"46\"],\n[-37.8976678167, 175.5356030833, \"2/157\"],\n[-37.9025187, 175.53438235, \"3/60\"],\n[-37.8981575333, 175.5358635, \"1/158\"],\n[-37.9027801667, 175.5340282833, \"1/60\"],\n[-37.8979776333, 175.536204, \"2/158\"],\n[-37.9023776667, 175.5345600167, \"4/60\"],\n[-37.8853151833, 175.4743354167, \"6\"],\n[-37.8839096667, 175.4747028, \"17\"],\n[-37.8826408833, 175.4741236667, \"44\"],\n[-37.8835314167, 175.4746655333, \"21\"],\n[-37.88308945, 175.4737775333, \"28\"],\n[-37.8814453833, 175.4749238167, \"41\"],\n[-37.8829200333, 175.4741641167, \"40\"],\n[-37.8804728833, 175.4739323833, \"66\"],\n[-37.8824228667, 175.47455335, \"33\"],\n[-37.8849254333, 175.4748141333, \"9\"],\n[-37.8815074833, 175.4735321333, \"54A\"],\n[-37.8829310333, 175.4736918167, \"38\"],\n[-37.8836627167, 175.47426195, \"20\"],\n[-37.8818192, 175.4745435833, \"35\"],\n[-37.8816592833, 175.4740403, \"48\"],\n[-37.8816905333, 175.4745296167, \"37\"],\n[-37.8835084, 175.47420785, \"22\"],\n[-37.8810922833, 175.4739643833, \"58\"],\n[-37.8814526, 175.4737812167, \"52\"],\n[-37.8813118333, 175.4739949167, \"56\"],\n[-37.88347125, 175.4749025167, \"21A\"],\n[-37.88493785, 175.4743158, \"10\"],\n[-37.88273915, 175.4748604167, \"29A\"],\n[-37.8837637667, 175.4746886167, \"19\"],\n[-37.8833199667, 175.47417425, \"24\"],\n[-37.8838467, 175.4742672333, \"18\"],\n[-37.8833191167, 175.4746548333, \"23\"],\n[-37.8845514833, 175.4742861, \"14\"],\n[-37.8853397, 175.4748736833, \"5\"],\n[-37.8847559333, 175.4748089167, \"11\"],\n[-37.8829521167, 175.47379525, \"36\"],\n[-37.88450605, 175.4747736667, \"13\"],\n[-37.8851099333, 175.4747525667, \"7\"],\n[-37.8847199, 175.4743100333, \"12\"],\n[-37.8831416, 175.4741727333, \"26\"],\n[-37.8808951333, 175.47394745, \"60\"],\n[-37.8808293167, 175.4744251333, \"49\"],\n[-37.8806857667, 175.4739440333, \"62\"],\n[-37.8815219, 175.4745206667, \"39\"],\n[-37.88054725, 175.47393465, \"64\"],\n[-37.8851369667, 175.4743293333, \"8\"],\n[-37.8829415167, 175.4735765833, \"34\"],\n[-37.8813163167, 175.4745132667, \"45\"],\n[-37.8825621, 175.4745672, \"31\"],\n[-37.882743, 175.4745888, \"29\"],\n[-37.8849588833, 175.4740735333, \"10A\"],\n[-37.8830033833, 175.47362085, \"32\"],\n[-37.8814134833, 175.4735401, \"54\"],\n[-37.8829174167, 175.4746101833, \"27\"],\n[-37.881329, 175.4749196167, \"43\"],\n[-37.8818477833, 175.4741495167, \"46\"],\n[-37.8806645833, 175.474444, \"51\"],\n[-37.8835542, 175.4740686833, \"22A\"],\n[-37.8815817333, 175.4735421833, \"54B\"],\n[-37.8840175333, 175.4742836, \"16\"],\n[-37.8827733167, 175.4741382833, \"42\"],\n[-37.8831040167, 175.4746167333, \"25\"],\n[-37.88152945, 175.47402335, \"50\"],\n[-37.8810197, 175.4744641667, \"47\"],\n[-37.90035815, 175.4810595167, \"6\"],\n[-37.9004626167, 175.48148175, \"5\"],\n[-37.9004783333, 175.4807893167, \"8\"],\n[-37.90053035, 175.48110305, \"10\"],\n[-37.9001194333, 175.4813414167, \"1\"],\n[-37.9002854167, 175.4813794, \"3\"],\n[-37.9005929833, 175.4812279167, \"12\"],\n[-37.9005808667, 175.4813900667, \"7\"],\n[-37.90019795, 175.4810457333, \"4\"],\n[-37.9000177833, 175.4810738667, \"2\"],\n[-37.8970709833, 175.471412, \"47\"],\n[-37.8962267, 175.4699139833, \"25\"],\n[-37.8957546, 175.46976195, \"14\"],\n[-37.8960248333, 175.4695108167, \"15\"],\n[-37.8958360833, 175.4699171167, \"16\"],\n[-37.8960595667, 175.4695834167, \"17\"],\n[-37.8956411833, 175.46957925, \"12\"],\n[-37.8955700333, 175.46945925, \"10\"],\n[-37.8957725, 175.4690580833, \"13\"],\n[-37.8957180833, 175.4689838167, \"11\"],\n[-37.89593735, 175.4700870667, \"18\"],\n[-37.8960019667, 175.4702152333, \"20\"],\n[-37.8960897167, 175.4696367833, \"21\"],\n[-37.8960816667, 175.4703379833, \"22\"],\n[-37.8961728667, 175.4698312667, \"23\"],\n[-37.89611545, 175.4703987167, \"24\"],\n[-37.89616535, 175.4704619, \"26\"],\n[-37.8965143333, 175.4704345833, \"31\"],\n[-37.8965995, 175.4706005, \"33\"],\n[-37.8952596167, 175.46889415, \"2\"],\n[-37.8954856333, 175.4693051333, \"8\"],\n[-37.8953389167, 175.4690297167, \"4\"],\n[-37.8953875833, 175.469136, \"6\"],\n[-37.9194040167, 175.4783079, \"10\"],\n[-37.9197726333, 175.4782003, \"12\"],\n[-37.9194026167, 175.4796128167, \"133\"],\n[-37.9197761667, 175.47866625, \"40\"],\n[-37.9199951833, 175.4798155667, \"148\"],\n[-37.91981195, 175.47907645, \"78\"],\n[-37.9194262833, 175.4788171833, \"59\"],\n[-37.9196759333, 175.4798573833, \"154\"],\n[-37.9193148667, 175.48000435, \"155\"],\n[-37.9194418667, 175.479248, \"95\"],\n[-37.91985945, 175.47946575, \"114\"],\n[-37.8948282, 175.4632178833, \"7\"],\n[-37.8948541333, 175.4634539, \"11\"],\n[-37.8946653333, 175.46382725, \"6A\"],\n[-37.8942238167, 175.4629336667, \"1A\"],\n[-37.8942486167, 175.4635523667, \"2\"],\n[-37.8942641667, 175.4631693, \"1\"],\n[-37.8950186, 175.4640242, \"8C\"],\n[-37.8944589, 175.4631798333, \"3\"],\n[-37.8946294333, 175.4631902, \"5\"],\n[-37.89450815, 175.4635785333, \"4\"],\n[-37.8946479, 175.4635948, \"6\"],\n[-37.89498065, 175.4637099667, \"8B\"],\n[-37.8948863333, 175.4637035833, \"8A\"],\n[-37.89496985, 175.46325115, \"9\"],\n[-37.8947899667, 175.4637024167, \"8\"],\n[-37.89513905, 175.4640248667, \"8D\"],\n[-37.9428737167, 175.4654487167, \"80\"],\n[-37.9435976167, 175.4662367333, \"85\"],\n[-37.9406954, 175.4650713667, \"60\"],\n[-37.9443210167, 175.466494, \"91\"],\n[-37.9430113833, 175.4661685167, \"79\"],\n[-37.9423613, 175.4660200167, \"73\"],\n[-37.9217937167, 175.54130465, \"1\"],\n[-37.9214646833, 175.5409056667, \"41\"],\n[-37.9210630667, 175.5409307333, \"86\"],\n[-37.9211853667, 175.5410841, \"66\"],\n[-37.92096145, 175.5405598333, \"83\"],\n[-37.9206217333, 175.54086065, \"133\"],\n[-37.9216795, 175.5411416833, \"21\"],\n[-37.9213075167, 175.5412473167, \"48\"],\n[-37.9214208167, 175.54139905, \"26\"],\n[-37.9204421, 175.5405807833, \"127\"],\n[-37.8771340667, 175.4766456167, \"17\"],\n[-37.87582545, 175.4764970667, \"2\"],\n[-37.8772717167, 175.47651465, \"19\"],\n[-37.87616025, 175.47643495, \"6\"],\n[-37.87713215, 175.4761358333, \"20\"],\n[-37.8763702833, 175.4764765, \"10\"],\n[-37.8768617333, 175.4759845333, \"18A\"],\n[-37.87599415, 175.4764346167, \"4\"],\n[-37.8767086, 175.4760073, \"18B\"],\n[-37.8762125167, 175.4761083833, \"8\"],\n[-37.8771747, 175.4758428833, \"22\"],\n[-37.8774091, 175.4763950167, \"21\"],\n[-37.8764912333, 175.4760901, \"18C\"],\n[-37.8775394167, 175.47599195, \"25\"],\n[-37.8765312333, 175.47650495, \"12\"],\n[-37.8774804667, 175.47621635, \"23\"],\n[-37.8767157833, 175.4764832833, \"14\"],\n[-37.8776499, 175.4754748667, \"26B\"],\n[-37.8769073667, 175.4763486167, \"16\"],\n[-37.8774781667, 175.4755253167, \"26A\"],\n[-37.8769741333, 175.4767701, \"15\"],\n[-37.8761039, 175.4768351333, \"3\"],\n[-37.8767738333, 175.4768931333, \"13\"],\n[-37.8775332833, 175.4757816833, \"27\"],\n[-37.87661205, 175.4769204667, \"11\"],\n[-37.8773297333, 175.4756777167, \"24\"],\n[-37.8761442833, 175.47743145, \"5B\"],\n[-37.8761289667, 175.4772156167, \"5A\"],\n[-37.87642575, 175.47690055, \"7\"],\n[-37.87587225, 175.4768522667, \"1\"],\n[-37.8762655667, 175.4774301667, \"5C\"],\n[-37.8765258167, 175.4771421167, \"9\"],\n[-37.8775364167, 175.48049705, \"9\"],\n[-37.87721065, 175.48073405, \"3\"],\n[-37.8773645167, 175.48035315, \"6\"],\n[-37.8769231833, 175.4805237833, \"2\"],\n[-37.8776686, 175.4807239333, \"7\"],\n[-37.87711275, 175.4803929167, \"4\"],\n[-37.8774604167, 175.4807430667, \"5\"],\n[-37.87709625, 175.4808098667, \"1\"],\n[-37.8775519167, 175.48027995, \"8\"],\n[-37.8028734167, 175.5544968333, \"64\"],\n[-37.7908094, 175.55717225, \"231\"],\n[-37.8020794667, 175.5538755, \"79\"],\n[-37.8025875667, 175.5538326167, \"73\"],\n[-37.8788754667, 175.4246207833, \"26\"],\n[-37.8579076333, 175.423369, \"257A\"],\n[-37.8799908833, 175.4247353667, \"14\"],\n[-37.8576267333, 175.4217187, \"257D\"],\n[-37.8792310833, 175.4241302167, \"21\"],\n[-37.8560683833, 175.4248801833, \"278\"],\n[-37.8737989833, 175.42415875, \"79\"],\n[-37.8813433667, 175.42936065, \"8/8\"],\n[-37.8734102333, 175.4246495, \"86\"],\n[-37.8605645333, 175.42419905, \"229\"],\n[-37.8719786333, 175.4241769, \"91\"],\n[-37.8781270333, 175.4246374167, \"34\"],\n[-37.8720274833, 175.4248217833, \"94\"],\n[-37.8578687167, 175.4212873833, \"257C\"],\n[-37.87800375, 175.4263720167, \"40\"],\n[-37.8618965833, 175.4242119333, \"213\"],\n[-37.8801869167, 175.42773835, \"1/8\"],\n[-37.8779240167, 175.42463595, \"36\"],\n[-37.8743793333, 175.4246104667, \"76\"],\n[-37.8578661, 175.4218172, \"257B\"],\n[-37.8539269833, 175.4240546833, \"293\"],\n[-37.8767564, 175.4218264667, \"49\"],\n[-37.8598008833, 175.4242299667, \"235\"],\n[-37.8584386667, 175.42421145, \"249\"],\n[-37.8570034667, 175.4213275, \"265\"],\n[-37.8800888, 175.42631445, \"10\"],\n[-37.8771395667, 175.4240152, \"43\"],\n[-37.8781435167, 175.4240972, \"33\"],\n[-37.8767499333, 175.4247563, \"52\"],\n[-37.87567625, 175.42459055, \"60\"],\n[-37.8807013, 175.4246087167, \"6\"],\n[-37.8764337167, 175.42402455, \"55\"],\n[-37.8645742, 175.4242096833, \"183\"],\n[-37.88046835, 175.4241622833, \"7\"],\n[-37.8700695833, 175.4241415, \"109\"],\n[-37.87820565, 175.4227695333, \"31\"],\n[-37.86436995, 175.42472575, \"184\"],\n[-37.8712348833, 175.4199563167, \"129\"],\n[-37.8655708833, 175.4241920833, \"163\"],\n[-37.866108, 175.4247113333, \"168\"],\n[-37.85758825, 175.4248484667, \"258\"],\n[-37.8806747167, 175.4289119667, \"3/8\"],\n[-37.8808697333, 175.4291759167, \"5/8\"],\n[-37.8805498667, 175.4315314333, \"7/8\"],\n[-37.8805142333, 175.4283195667, \"8\"],\n[-37.8642561, 175.4211131, \"185B\"],\n[-37.8642016333, 175.4241768167, \"185C\"],\n[-37.8584019167, 175.4247333, \"254\"],\n[-37.8579075, 175.4247216333, \"256\"],\n[-37.85733205, 175.4242334, \"261\"],\n[-37.8572177833, 175.4231895, \"263\"],\n[-37.8643650167, 175.4220877667, \"185A\"],\n[-37.89998625, 175.4831132833, \"33\"],\n[-37.9028015, 175.4824331833, \"62A\"],\n[-37.9027069833, 175.4824823167, \"60A\"],\n[-37.8999237833, 175.48395105, \"28\"],\n[-37.8998797667, 175.4835940667, \"30\"],\n[-37.9000587333, 175.4835154167, \"32\"],\n[-37.9016412667, 175.48230395, \"51\"],\n[-37.901574, 175.4827421167, \"52\"],\n[-37.90194355, 175.4826111667, \"54\"],\n[-37.90215485, 175.4824878833, \"56\"],\n[-37.9003890667, 175.48224575, \"41\"],\n[-37.9005753833, 175.4833095667, \"42\"],\n[-37.9004801833, 175.4824322, \"43\"],\n[-37.90073175, 175.48323555, \"44\"],\n[-37.9007203833, 175.4827551667, \"45\"],\n[-37.9008984833, 175.4831199, \"46\"],\n[-37.9010707, 175.4830293333, \"48\"],\n[-37.9010407167, 175.4825866833, \"49\"],\n[-37.90236485, 175.4823782167, \"58\"],\n[-37.9025425, 175.4818774167, \"59\"],\n[-37.9025757167, 175.4822757667, \"60\"],\n[-37.9029402833, 175.4816723667, \"61\"],\n[-37.90278895, 175.4821800333, \"62\"],\n[-37.9031374667, 175.4822319333, \"64A\"],\n[-37.9030315833, 175.4820359667, \"64\"],\n[-37.9032178667, 175.4819789333, \"66\"],\n[-37.9019434, 175.48286115, \"54A\"],\n[-37.8818104333, 175.4707039833, \"12\"],\n[-37.8814851833, 175.4704820167, \"9\"],\n[-37.8810768333, 175.4699034, \"5B\"],\n[-37.8818488, 175.4702216, \"1\"],\n[-37.8816215667, 175.47092875, \"11\"],\n[-37.8816941833, 175.4701273667, \"2\"],\n[-37.8811842333, 175.4704525667, \"7\"],\n[-37.8813522667, 175.47002245, \"4\"],\n[-37.88194605, 175.4707180167, \"13\"],\n[-37.8813308, 175.4704956333, \"8\"],\n[-37.88115305, 175.4702513333, \"6\"],\n[-37.8811936167, 175.4700739, \"5A\"],\n[-37.8815373167, 175.4700831, \"3\"],\n[-37.8816521333, 175.4705540833, \"10\"],\n[-37.8088981167, 175.36906915, \"77\"],\n[-37.8088042833, 175.3655253833, \"80\"],\n[-37.8066999167, 175.3689073333, \"52\"],\n[-37.8042113, 175.3710341667, \"21\"],\n[-37.8035833, 175.3699288833, \"17\"],\n[-37.8043107833, 175.3697591667, \"23\"],\n[-37.8108776667, 175.3698460833, \"87B\"],\n[-37.8045451333, 175.3697163333, \"25\"],\n[-37.8104246167, 175.365628, \"94B\"],\n[-37.8047750833, 175.3692679333, \"30\"],\n[-37.80400095, 175.3716803333, \"19B\"],\n[-37.8025388833, 175.3693096833, \"2\"],\n[-37.8032552333, 175.3718025, \"19A\"],\n[-37.8027435333, 175.3695240333, \"4\"],\n[-37.8105842, 175.3663406667, \"94A\"],\n[-37.8028778167, 175.3696653833, \"6\"],\n[-37.8095059667, 175.37026555, \"81\"],\n[-37.8055475667, 175.3691296833, \"36\"],\n[-37.8101165833, 175.3695157, \"87A\"],\n[-37.8055532333, 175.3695431833, \"41\"],\n[-37.8068988, 175.3688744333, \"54\"],\n[-37.8059911667, 175.3690365333, \"44\"],\n[-37.8074588333, 175.36921695, \"59\"],\n[-37.8075005667, 175.3687474667, \"60\"],\n[-37.80836585, 175.3685797833, \"64\"],\n[-37.8060896833, 175.3724944167, \"67A\"],\n[-37.8079139, 175.3722770833, \"67B\"],\n[-37.8089385167, 175.3721465167, \"67C\"],\n[-37.81012905, 175.37175445, \"67D\"],\n[-37.8110090333, 175.3680073333, \"96\"],\n[-37.8103251, 175.3687031333, \"89\"],\n[-37.8101581667, 175.36820355, \"88\"],\n[-37.8097519333, 175.36879765, \"85\"],\n[-37.8084821, 175.37132025, \"69\"],\n[-37.8818073167, 175.4679643, \"6\"],\n[-37.8823003833, 175.4676866167, \"1\"],\n[-37.8818464333, 175.4675593, \"5\"],\n[-37.8822481333, 175.4679646833, \"2\"],\n[-37.8817913833, 175.4677682167, \"7\"],\n[-37.8821612333, 175.4674131167, \"3A\"],\n[-37.8821403, 175.46766325, \"3\"],\n[-37.8821458833, 175.4679535, \"4\"],\n[-37.9077193667, 175.4708605833, \"32\"],\n[-37.9105951, 175.4793621667, \"98A\"],\n[-37.9066916333, 175.4685963, \"19\"],\n[-37.9104376833, 175.4794448167, \"98\"],\n[-37.9064596833, 175.4678557333, \"11\"],\n[-37.9103041333, 175.479528, \"100\"],\n[-37.9090584167, 175.477976, \"83\"],\n[-37.9088679, 175.4779112, \"81B\"],\n[-37.90751235, 175.4701317833, \"26\"],\n[-37.9085779167, 175.4748978833, \"57B\"],\n[-37.9077844167, 175.4710735, \"34\"],\n[-37.9088840833, 175.4747589333, \"60\"],\n[-37.9063916833, 175.4676261333, \"9\"],\n[-37.9091791167, 175.47510015, \"64A\"],\n[-37.9090063, 175.4778273167, \"81\"],\n[-37.9094076667, 175.4749288167, \"66\"],\n[-37.9104649333, 175.4812665333, \"111\"],\n[-37.9094658167, 175.47513735, \"66A\"],\n[-37.9106371667, 175.4806045833, \"108\"],\n[-37.9085164667, 175.47469955, \"57A\"],\n[-37.9065306333, 175.4680931667, \"13B\"],\n[-37.90744655, 175.46992735, \"24\"],\n[-37.90669875, 175.4675042833, \"10\"],\n[-37.9063321167, 175.467451, \"7\"],\n[-37.9068232667, 175.4690555333, \"23\"],\n[-37.9064476333, 175.4682054167, \"13A\"],\n[-37.90728695, 175.4687101167, \"20A\"],\n[-37.90564665, 175.4665080833, \"1B\"],\n[-37.9095425167, 175.4780996, \"87\"],\n[-37.9055099333, 175.4665785667, \"1C\"],\n[-37.9064278333, 175.4685453667, \"17\"],\n[-37.9087755667, 175.4730346, \"48A\"],\n[-37.9092946833, 175.4772395333, \"77\"],\n[-37.9089289833, 175.4729708, \"48B\"],\n[-37.9070766, 175.4680242167, \"14A\"],\n[-37.9099299333, 175.4794760167, \"97\"],\n[-37.9060196667, 175.4664193167, \"3\"],\n[-37.9093701833, 175.4764340333, \"78\"],\n[-37.9088882, 175.4759446333, \"71\"],\n[-37.9071022833, 175.4688659833, \"20\"],\n[-37.9087805333, 175.4732292667, \"50A\"],\n[-37.9101438333, 175.4802177167, \"103\"],\n[-37.9100409167, 175.4805634167, \"105A\"],\n[-37.9103943, 175.4797851, \"102\"],\n[-37.9097405333, 175.4787679167, \"95\"],\n[-37.9093126667, 175.4762224667, \"76\"],\n[-37.9076596, 175.4706490167, \"30\"],\n[-37.9090636667, 175.4764674667, \"75\"],\n[-37.9091701167, 175.47836235, \"89\"],\n[-37.9089457333, 175.4761631167, \"73\"],\n[-37.9085522833, 175.4736826, \"52\"],\n[-37.9092411833, 175.4760100667, \"74\"],\n[-37.9079714833, 175.4729381333, \"45\"],\n[-37.9080979833, 175.4733453333, \"49\"],\n[-37.9069515333, 175.4683180833, \"16\"],\n[-37.9066421833, 175.4673182, \"8\"],\n[-37.90937045, 175.47757155, \"79\"],\n[-37.9081895333, 175.47364565, \"51\"],\n[-37.9086237333, 175.4750583333, \"59\"],\n[-37.9091840167, 175.4757918333, \"72\"],\n[-37.9087522333, 175.47547835, \"63\"],\n[-37.90689515, 175.46927105, \"25\"],\n[-37.9078795, 175.4713550833, \"36\"],\n[-37.9056039, 175.4664057333, \"1A\"],\n[-37.9106886833, 175.4808305833, \"110\"],\n[-37.9059434167, 175.4661582833, \"1\"],\n[-37.90686465, 175.4680447333, \"14\"],\n[-37.9100754167, 175.4799441333, \"101\"],\n[-37.9096307667, 175.4784052167, \"93\"],\n[-37.9103163333, 175.4807391667, \"107\"],\n[-37.9099907833, 175.47967445, \"99\"],\n[-37.9102545333, 175.4804980667, \"105\"],\n[-37.9067854667, 175.46778495, \"12\"],\n[-37.9087875, 175.4761555333, \"73A\"],\n[-37.9088226667, 175.4757288667, \"69\"],\n[-37.90860015, 175.4738656333, \"54\"],\n[-37.9084004667, 175.4731332667, \"48\"],\n[-37.9086524, 175.4734778333, \"52A\"],\n[-37.9070271167, 175.4685905333, \"18\"],\n[-37.9075912333, 175.47002045, \"26A\"],\n[-37.9089373833, 175.4749522333, \"62\"],\n[-37.90542545, 175.4648276667, \"2\"],\n[-37.9075973667, 175.4703872167, \"28\"],\n[-37.9084245, 175.4757077667, \"65\"],\n[-37.90912645, 175.4755800667, \"70\"],\n[-37.90927015, 175.4751967833, \"68A\"],\n[-37.9094690167, 175.4778629667, \"85\"],\n[-37.90713625, 175.4682970167, \"16A\"],\n[-37.9084881667, 175.4734576333, \"50\"],\n[-37.9071228167, 175.4684523333, \"18A\"],\n[-37.9066047833, 175.4683209, \"15\"],\n[-37.9086888833, 175.4752875667, \"61\"],\n[-37.9092166833, 175.4785048333, \"91\"],\n[-37.90675295, 175.4688188167, \"21\"],\n[-37.9089505167, 175.4731089667, \"50B\"],\n[-37.9090581667, 175.4753575833, \"68\"],\n[-37.9086879167, 175.47334805, \"50C\"],\n[-37.91039185, 175.48101575, \"109\"],\n[-37.9091127833, 175.4786399833, \"91A\"],\n[-37.90897975, 175.4751060667, \"64\"],\n[-37.8717186833, 175.4615598167, \"102\"],\n[-37.8620697167, 175.4558799667, \"224\"],\n[-37.8767515, 175.4619986833, \"42\"],\n[-37.8664140333, 175.4578848167, \"168\"],\n[-37.8778354833, 175.46234395, \"32\"],\n[-37.8624631167, 175.456142, \"216\"],\n[-37.8639001667, 175.4568719667, \"194\"],\n[-37.8719572167, 175.4602589, \"100\"],\n[-37.87530725, 175.46140325, \"60\"],\n[-37.86265385, 175.4563095333, \"214\"],\n[-37.8736987167, 175.46078595, \"80\"],\n[-37.8899623167, 175.4632323, \"57A\"],\n[-37.8891142333, 175.4571846167, \"4\"],\n[-37.8896277, 175.4633464833, \"59\"],\n[-37.88913275, 175.4644998167, \"60\"],\n[-37.8891977, 175.46335, \"1/52\"],\n[-37.8902417, 175.4604661, \"25A\"],\n[-37.8892224667, 175.4630494333, \"50\"],\n[-37.8904468333, 175.4604248, \"25\"],\n[-37.8899309167, 175.45755835, \"7\"],\n[-37.8893669667, 175.4601698, \"26\"],\n[-37.8891862667, 175.4573128833, \"4B\"],\n[-37.8897543167, 175.4603741167, \"27A\"],\n[-37.8895259333, 175.4573211667, \"4A\"],\n[-37.8899523167, 175.4603841667, \"27B\"],\n[-37.8887959333, 175.4610821833, \"34\"],\n[-37.8901048833, 175.4603943833, \"27C\"],\n[-37.8891028833, 175.4650312333, \"64\"],\n[-37.88868315, 175.4633466833, \"52B\"],\n[-37.8896881, 175.4624856667, \"43\"],\n[-37.8899772333, 175.46296325, \"53\"],\n[-37.8893557167, 175.4604985333, \"28\"],\n[-37.8887644667, 175.46359225, \"54A\"],\n[-37.8887913667, 175.46338915, \"52A\"],\n[-37.8891714, 175.4635651833, \"54B\"],\n[-37.8898920167, 175.4582737, \"13\"],\n[-37.8891619167, 175.46382635, \"56\"],\n[-37.8902644667, 175.4602963, \"23B\"],\n[-37.8903580333, 175.4578762167, \"9A\"],\n[-37.8895457333, 175.4571203833, \"2\"],\n[-37.8899171667, 175.4577745667, \"9\"],\n[-37.8891239833, 175.4610409833, \"32\"],\n[-37.8886105167, 175.4627844833, \"48A\"],\n[-37.8897850667, 175.4601521667, \"23\"],\n[-37.8885782833, 175.4630716, \"48B\"],\n[-37.8895879667, 175.4639701, \"65\"],\n[-37.88875245, 175.4628892667, \"48C\"],\n[-37.8897637667, 175.46090555, \"29\"],\n[-37.8898831667, 175.4584686167, \"15\"],\n[-37.8900645833, 175.4630566333, \"57\"],\n[-37.8894477833, 175.4587900667, \"20\"],\n[-37.8898162, 175.4597956833, \"21\"],\n[-37.8903147667, 175.4585830833, \"15A\"],\n[-37.8899451, 175.4573311, \"5\"],\n[-37.8892625, 175.46226515, \"42\"],\n[-37.88959655, 175.4636994833, \"63\"],\n[-37.8888289, 175.4626433167, \"46B\"],\n[-37.8897502167, 175.4611882333, \"31\"],\n[-37.8890874833, 175.4653801667, \"66\"],\n[-37.8896453667, 175.4629410333, \"51\"],\n[-37.88871155, 175.4626789833, \"46A\"],\n[-37.8889027, 175.4648962833, \"64A\"],\n[-37.8888759167, 175.4633533167, \"2/52\"],\n[-37.8903299167, 175.4581553, \"13A\"],\n[-37.8890716, 175.4657335833, \"68\"],\n[-37.8894647167, 175.4584316167, \"16\"],\n[-37.8889404333, 175.4574026, \"6B\"],\n[-37.8896737667, 175.4626918667, \"45\"],\n[-37.8899074, 175.4580168, \"11\"],\n[-37.8898575333, 175.4588148833, \"17\"],\n[-37.8896982167, 175.46224375, \"1/41-7/41\"],\n[-37.8895164167, 175.4575203, \"6\"],\n[-37.8899765333, 175.4602187333, \"23A\"],\n[-37.8891143, 175.4647213833, \"62\"],\n[-37.8892440667, 175.46252665, \"46\"],\n[-37.88890735, 175.4607073833, \"30A\"],\n[-37.88930945, 175.4612971, \"36\"],\n[-37.8892202667, 175.4574350833, \"6A\"],\n[-37.8891678667, 175.4639653333, \"58\"],\n[-37.8898663667, 175.4589170833, \"19\"],\n[-37.88937355, 175.4599354833, \"24\"],\n[-37.8893259833, 175.4609626833, \"32A\"],\n[-37.8899845333, 175.4624851, \"49\"],\n[-37.8700170667, 175.4425962167, \"35\"],\n[-37.87002395, 175.4440538333, \"45\"],\n[-37.8703417167, 175.4408183833, \"19\"],\n[-37.8704475833, 175.44446245, \"48\"],\n[-37.8705703667, 175.44211805, \"32\"],\n[-37.8712876167, 175.4401329, \"8\"],\n[-37.8706072, 175.4431831333, \"40\"],\n[-37.88103245, 175.43996055, \"74\"],\n[-37.8810238833, 175.44054185, \"144\"],\n[-37.8819540833, 175.44341985, \"444\"],\n[-37.8810368167, 175.4414257833, \"218\"],\n[-37.8836569833, 175.4449454, \"695\"],\n[-37.8805499, 175.44146635, \"219\"],\n[-37.88153545, 175.4435217, \"409\"],\n[-37.8805337667, 175.4422020167, \"277\"],\n[-37.8810820333, 175.4423189667, \"302\"],\n[-37.8826985333, 175.4431283833, \"503\"],\n[-37.8825451667, 175.4439081167, \"533\"],\n[-37.8834376, 175.4454858167, \"694\"],\n[-37.8806721167, 175.44412345, \"397\"],\n[-37.81387095, 175.45409265, \"1\"],\n[-37.821195, 175.4616223833, \"104\"],\n[-37.8211624833, 175.4682833, \"181\"],\n[-37.8178846167, 175.4588240333, \"65\"],\n[-37.8149536667, 175.4553629333, \"15\"],\n[-37.8197732667, 175.4671436, \"167\"],\n[-37.8161526333, 175.4563465, \"35\"],\n[-37.8209648833, 175.4674674, \"178\"],\n[-37.820487, 175.4606511167, \"86\"],\n[-37.9711954833, 175.3673120333, \"66\"],\n[-37.8927382667, 175.4630076, \"2A\"],\n[-37.8930191833, 175.4631100333, \"1\"],\n[-37.89302745, 175.4629626333, \"1A\"],\n[-37.8927669333, 175.46308965, \"2\"],\n[-37.8929038667, 175.46322235, \"5\"],\n[-37.8926603333, 175.4633015, \"3A\"],\n[-37.89273305, 175.4631913833, \"3\"],\n[-37.8928261167, 175.4632172333, \"4\"],\n[-37.8863257333, 175.3892431333, \"29\"],\n[-37.8828649833, 175.3942711333, \"76\"],\n[-37.88740485, 175.3880108167, \"3\"],\n[-37.8816477833, 175.39495895, \"85\"],\n[-37.9193026667, 175.46865615, \"27\"],\n[-37.9191047667, 175.4689871, \"21\"],\n[-37.9190031667, 175.4685222, \"29\"],\n[-37.9191244667, 175.4666619167, \"55\"],\n[-37.9187559333, 175.4673880833, \"43\"],\n[-37.9191856, 175.4667804333, \"49\"],\n[-37.9183846667, 175.4653317833, \"75\"],\n[-37.9186045833, 175.46662185, \"59\"],\n[-37.9191840167, 175.4678865167, \"35\"],\n[-37.9191584333, 175.4662006333, \"61\"],\n[-37.9188624, 175.4676419833, \"39\"],\n[-37.9184419667, 175.4657698, \"69\"],\n[-37.9178364167, 175.4627168667, \"111\"],\n[-37.9185242667, 175.4661814167, \"63\"],\n[-37.9191998, 175.4694479333, \"17\"],\n[-37.9190388167, 175.4654450833, \"71\"],\n[-37.91796485, 175.4632720833, \"103\"],\n[-37.91880005, 175.4653678333, \"73\"],\n[-37.9182357167, 175.4645811, \"93\"],\n[-37.9177229333, 175.4619539, \"119\"],\n[-37.91802955, 175.4624559167, \"115\"],\n[-37.9176650833, 175.4616779667, \"123\"],\n[-37.91870075, 175.4670770833, \"45\"],\n[-37.9176143667, 175.4613274, \"125\"],\n[-37.9189086667, 175.468155, \"33\"],\n[-37.9174097167, 175.4613318833, \"127\"],\n[-37.9190924, 175.4675234333, \"41\"],\n[-37.8068523167, 175.3939250833, \"5\"],\n[-37.8144720167, 175.4032648333, \"122\"],\n[-37.8070121667, 175.3941206333, \"7\"],\n[-37.8135841833, 175.40160315, \"114\"],\n[-37.8071723, 175.39433025, \"9\"],\n[-37.8073319667, 175.3945375, \"13\"],\n[-37.8088248333, 175.39658935, \"35\"],\n[-37.8150867, 175.4041908167, \"138\"],\n[-37.8087098667, 175.39797025, \"45\"],\n[-37.8153430833, 175.4043867333, \"140\"],\n[-37.8113713333, 175.3997835833, \"77\"],\n[-37.8128633667, 175.4026050333, \"103\"],\n[-37.8153182333, 175.404702, \"142\"],\n[-37.8136947667, 175.4026962667, \"113\"],\n[-37.8146717833, 175.4042277167, \"133\"],\n[-37.8132897167, 175.40224345, \"105\"],\n[-37.87318525, 175.5752061, \"783\"],\n[-37.87086625, 175.5605686167, \"3/668\"],\n[-37.8736095333, 175.5759500667, \"791\"],\n[-37.8846973333, 175.5128454333, \"105\"],\n[-37.882909, 175.5284399, \"295\"],\n[-37.8854251167, 175.50715345, \"42\"],\n[-37.8730277, 175.5735052333, \"768\"],\n[-37.8852305667, 175.5138063667, \"116\"],\n[-37.8765950833, 175.5441586167, \"457\"],\n[-37.8854819167, 175.51593135, \"130\"],\n[-37.8793962333, 175.5400538, \"398\"],\n[-37.8856814667, 175.5176310333, \"146\"],\n[-37.8850541667, 175.5122725, \"94\"],\n[-37.8859048, 175.5196622667, \"158\"],\n[-37.8839926333, 175.5070946667, \"44\"],\n[-37.8828246833, 175.5299873833, \"300\"],\n[-37.8835640333, 175.5073463, \"47\"],\n[-37.8812550167, 175.53308795, \"338\"],\n[-37.87624165, 175.5468433, \"472\"],\n[-37.8735672333, 175.5726554167, \"2/766\"],\n[-37.8756854833, 175.5468615167, \"483\"],\n[-37.8855889333, 175.5069699, \"1/42\"],\n[-37.8764352833, 175.55027325, \"508\"],\n[-37.88626155, 175.5222170833, \"2/182\"],\n[-37.8739916667, 175.5532237833, \"537\"],\n[-37.8790302167, 175.5397931667, \"396\"],\n[-37.8750138, 175.5530649833, \"542\"],\n[-37.8839209333, 175.5106114167, \"79\"],\n[-37.8790556833, 175.5364847667, \"371\"],\n[-37.8804106667, 175.5347674167, \"348\"],\n[-37.8843208, 175.50806425, \"50\"],\n[-37.8835448667, 175.50621025, \"37\"],\n[-37.8789683667, 175.5407611667, \"400\"],\n[-37.8819428333, 175.5302822333, \"307\"],\n[-37.8838573833, 175.5041196833, \"26\"],\n[-37.8728271833, 175.5730276667, \"1/766\"],\n[-37.8862517333, 175.5218569167, \"1/182\"],\n[-37.88592065, 175.52030825, \"170\"],\n[-37.8804488333, 175.5336723833, \"345\"],\n[-37.8724145667, 175.5722238833, \"756\"],\n[-37.8710689167, 175.5600702167, \"2/668\"],\n[-37.8749418667, 175.5762505333, \"808\"],\n[-37.88330185, 175.5037989167, \"25\"],\n[-37.8742985667, 175.5709469667, \"766\"],\n[-37.8855461833, 175.5169524167, \"138\"],\n[-37.8840771833, 175.5086316, \"57\"],\n[-37.8799641333, 175.5345286833, \"351\"],\n[-37.8747767833, 175.5703700833, \"4/766\"],\n[-37.8712547, 175.5595212, \"1/668\"],\n[-37.8852620333, 175.5203346333, \"167\"],\n[-37.87846535, 175.5402971, \"407\"],\n[-37.87451705, 175.5722616833, \"3/766\"],\n[-37.8865716667, 175.5064284333, \"2/42\"],\n[-37.8699697833, 175.5701625667, \"725\"],\n[-37.8850022833, 175.5119347333, \"92\"],\n[-37.9141589833, 175.4676254, \"19\"],\n[-37.9155973833, 175.46772575, \"11/2\"],\n[-37.91552955, 175.4660865, \"36/2\"],\n[-37.9156190333, 175.4673462667, \"9/2\"],\n[-37.9154039167, 175.4660434167, \"35/2\"],\n[-37.9155738333, 175.4664463167, \"5/2\"],\n[-37.91535915, 175.4658985167, \"34/2\"],\n[-37.9155678833, 175.4667106333, \"6/2\"],\n[-37.91544365, 175.4658299, \"33/2\"],\n[-37.9160503333, 175.4675822, \"21/2\"],\n[-37.9156064333, 175.4657751333, \"32/2\"],\n[-37.9147495167, 175.4662775167, \"5\"],\n[-37.9157566167, 175.46569785, \"31/2\"],\n[-37.9154667667, 175.4679029167, \"12/2\"],\n[-37.91585185, 175.4657343333, \"30/2\"],\n[-37.9152016333, 175.4653091167, \"1\"],\n[-37.9153522167, 175.4663997833, \"4\"],\n[-37.9156452333, 175.4675400833, \"10/2\"],\n[-37.9153521167, 175.4665981833, \"4A\"],\n[-37.91558475, 175.4671245833, \"8/2\"],\n[-37.9153486333, 175.4667996333, \"6\"],\n[-37.9155721333, 175.4669092, \"7/2\"],\n[-37.9152996667, 175.4669875833, \"6A\"],\n[-37.9150243, 175.46636625, \"5A\"],\n[-37.9162041333, 175.4675381333, \"22/2\"],\n[-37.9162005, 175.4673120333, \"23/2\"],\n[-37.9161440167, 175.4671061, \"24/2\"],\n[-37.9160962667, 175.4669004667, \"25/2\"],\n[-37.9160486333, 175.4666893333, \"26/2\"],\n[-37.9159963833, 175.46648905, \"27/2\"],\n[-37.91591785, 175.4662933333, \"28/2\"],\n[-37.9157758667, 175.4662000667, \"38/2\"],\n[-37.9157248667, 175.4659447167, \"37/2\"],\n[-37.9158606, 175.46592755, \"29/2\"],\n[-37.9159540833, 175.4673968833, \"20/2\"],\n[-37.91578295, 175.4674348167, \"17/2\"],\n[-37.91584205, 175.4677180167, \"18/2\"],\n[-37.9156962333, 175.4664396833, \"13/2\"],\n[-37.915699, 175.4667043333, \"14/2\"],\n[-37.9157030167, 175.4669139, \"15/2\"],\n[-37.9157118833, 175.4671016, \"16/2\"],\n[-37.9158802833, 175.4671848, \"19/2\"],\n[-37.9146419333, 175.4678385333, \"16\"],\n[-37.9149667, 175.46605435, \"3\"],\n[-37.9150279333, 175.4667922833, \"7\"],\n[-37.9141704333, 175.4678512833, \"21\"],\n[-37.9142653, 175.4673118833, \"15\"],\n[-37.91450075, 175.4674330833, \"13\"],\n[-37.9147242667, 175.46734925, \"11\"],\n[-37.9148913167, 175.4671297667, \"9\"],\n[-37.9151929833, 175.4672369167, \"8\"],\n[-37.9144812833, 175.4679524833, \"18\"],\n[-37.9149502167, 175.4675873667, \"12\"],\n[-37.9147979333, 175.4677301167, \"14\"],\n[-37.9140953833, 175.4682407667, \"22\"],\n[-37.9143135333, 175.4679806667, \"20\"],\n[-37.9139027667, 175.4673990667, \"17\"],\n[-37.91508455, 175.46743825, \"10\"],\n[-37.8207364833, 175.3925286333, \"110\"],\n[-37.8140913167, 175.3867786167, \"31\"],\n[-37.82388165, 175.3961648833, \"164\"],\n[-37.8149241833, 175.3869723167, \"38\"],\n[-37.8341317167, 175.4133935833, \"364\"],\n[-37.8244762167, 175.39859475, \"186\"],\n[-37.82108675, 175.3928284333, \"112\"],\n[-37.8257318, 175.4028302167, \"236\"],\n[-37.8213587333, 175.3930749333, \"114\"],\n[-37.8245402667, 175.3979335833, \"184\"],\n[-37.8216469333, 175.3939728667, \"129\"],\n[-37.8287610333, 175.4083265833, \"302\"],\n[-37.82049075, 175.3882675333, \"92\"],\n[-37.8209210167, 175.3876718333, \"90B\"],\n[-37.8243807167, 175.4009181, \"207\"],\n[-37.8210754833, 175.3869467167, \"90A\"],\n[-37.8142259167, 175.3862871667, \"26\"],\n[-37.8142241, 175.3891889667, \"43\"],\n[-37.8355967167, 175.4144058667, \"382\"],\n[-37.8263508333, 175.4048937333, \"251\"],\n[-37.8237985833, 175.3972016667, \"173\"],\n[-37.82187925, 175.3941376333, \"137\"],\n[-37.8159891667, 175.3862058833, \"42\"],\n[-37.8233059333, 175.3950369833, \"156\"],\n[-37.8224871333, 175.3939314167, \"148\"],\n[-37.8194135833, 175.3914179833, \"100\"],\n[-37.8149244, 175.38763835, \"41\"],\n[-37.8192395667, 175.3919598167, \"103\"],\n[-37.8202387167, 175.3879586, \"90D\"],\n[-37.81610515, 175.3888637333, \"59\"],\n[-37.8282481667, 175.4077297, \"290\"],\n[-37.81714775, 175.3892465833, \"68\"],\n[-37.8196078, 175.3887699667, \"90\"],\n[-37.817284, 175.39001715, \"77\"],\n[-37.81837465, 175.39045065, \"86\"],\n[-37.8277749, 175.4071191667, \"272\"],\n[-37.8265916333, 175.4056707167, \"251A\"],\n[-37.8128841167, 175.3855143833, \"11\"],\n[-37.8291297, 175.4098336167, \"313\"],\n[-37.8329969667, 175.4126325333, \"358\"],\n[-37.8322266, 175.4129321167, \"347\"],\n[-37.8348266833, 175.4139054, \"372\"],\n[-37.9099726167, 175.4757886667, \"71\"],\n[-37.9101903333, 175.4760631833, \"73\"],\n[-37.9152217167, 175.47371395, \"131\"],\n[-37.9059390167, 175.47854405, \"32\"],\n[-37.9097995667, 175.4753924833, \"71B\"],\n[-37.9044269833, 175.4792423333, \"12\"],\n[-37.9084252667, 175.4764496833, \"49\"],\n[-37.9086711167, 175.4772269833, \"52\"],\n[-37.9116987, 175.4759278667, \"92\"],\n[-37.9098638167, 175.47581795, \"69\"],\n[-37.9117964333, 175.4757621167, \"94\"],\n[-37.90989485, 175.47559865, \"71A\"],\n[-37.9073590833, 175.4778494167, \"40A\"],\n[-37.91330765, 175.47552755, \"108\"],\n[-37.9057401833, 175.4781938167, \"25\"],\n[-37.911296, 175.4748848167, \"89\"],\n[-37.9121108667, 175.4758530667, \"98A\"],\n[-37.9043787833, 175.4787889167, \"13\"],\n[-37.9083076667, 175.4766785667, \"47A\"],\n[-37.9036909333, 175.4791402667, \"3\"],\n[-37.91220345, 175.4755294, \"100\"],\n[-37.9053728833, 175.4783509333, \"1/23\"],\n[-37.90519915, 175.4784179, \"21\"],\n[-37.9052690667, 175.4783896667, \"21A\"],\n[-37.9114245, 175.4759148333, \"88\"],\n[-37.9081505833, 175.4774655, \"46\"],\n[-37.9142982833, 175.4745833, \"120\"],\n[-37.9131166, 175.4751412333, \"106\"],\n[-37.9083510667, 175.47694365, \"47\"],\n[-37.9065192667, 175.4778253667, \"31\"],\n[-37.9083240333, 175.47739375, \"48\"],\n[-37.9149526833, 175.4742867833, \"126\"],\n[-37.9103091667, 175.476032, \"75\"],\n[-37.9118749833, 175.4749764833, \"101A\"],\n[-37.9042161667, 175.4788623333, \"11\"],\n[-37.9129278167, 175.4752157333, \"104\"],\n[-37.9121256667, 175.4751926333, \"103\"],\n[-37.9112547, 175.4755432667, \"85\"],\n[-37.9142550667, 175.4741810667, \"121\"],\n[-37.9106238667, 175.4759059667, \"77A\"],\n[-37.9104479, 175.4759570167, \"77\"],\n[-37.9114070667, 175.4754850833, \"87\"],\n[-37.9113298833, 175.47516835, \"87A\"],\n[-37.9148737833, 175.4738503833, \"127\"],\n[-37.9112868667, 175.4759815833, \"86\"],\n[-37.9111339667, 175.4760538, \"82\"],\n[-37.91156375, 175.4758556833, \"90\"],\n[-37.9151142, 175.4742192667, \"128\"],\n[-37.9151403167, 175.4737436, \"129\"],\n[-37.915375, 175.4740628167, \"130\"],\n[-37.9144834167, 175.4745128333, \"122\"],\n[-37.9144913, 175.4737651167, \"123A\"],\n[-37.9144983167, 175.4740186167, \"123\"],\n[-37.9146270167, 175.4744201833, \"124\"],\n[-37.9126748, 175.4749461833, \"105\"],\n[-37.9128134833, 175.4748703333, \"107\"],\n[-37.9129492333, 175.4747718167, \"109\"],\n[-37.9131005833, 175.4746998, \"111\"],\n[-37.9132571833, 175.4746269833, \"113\"],\n[-37.9135018833, 175.4749717, \"114\"],\n[-37.91340395, 175.4745515, \"115\"],\n[-37.9136698, 175.4748826167, \"116\"],\n[-37.91356065, 175.4744797, \"117\"],\n[-37.91379245, 175.4748357, \"118\"],\n[-37.9136715833, 175.4744300167, \"119\"],\n[-37.9085906833, 175.4768593, \"53\"],\n[-37.9089448167, 175.47752235, \"54A\"],\n[-37.9088353, 175.4771584333, \"54\"],\n[-37.9087287167, 175.4767663333, \"55\"],\n[-37.9090036667, 175.4770825167, \"56\"],\n[-37.9065108833, 175.4773228833, \"33\"],\n[-37.90671225, 175.4781606833, \"34\"],\n[-37.90673975, 175.4777032, \"35\"],\n[-37.9068835, 175.4780653833, \"36\"],\n[-37.9067380833, 175.4772301167, \"37\"],\n[-37.9070569167, 175.47798485, \"38\"],\n[-37.9070025333, 175.4775753333, \"39\"],\n[-37.90721945, 175.47790165, \"40\"],\n[-37.9072522667, 175.4774842833, \"41\"],\n[-37.9073576167, 175.47744775, \"43\"],\n[-37.9055054167, 175.4782415167, \"23\"],\n[-37.9146826833, 175.4739228, \"125\"],\n[-37.9048823167, 175.4785375333, \"17\"],\n[-37.9120084333, 175.4756561667, \"98\"],\n[-37.9086973167, 175.4775292833, \"52A\"],\n[-37.90495545, 175.4780925667, \"19A\"],\n[-37.9115964667, 175.4754098667, \"91\"],\n[-37.9117456333, 175.4753338667, \"93\"],\n[-37.9047131167, 175.47861435, \"15\"],\n[-37.9166350333, 175.47304345, \"141\"],\n[-37.9119624833, 175.4752623, \"101\"],\n[-37.9039384833, 175.4785719, \"9A\"],\n[-37.9040451, 175.4789520667, \"9\"],\n[-37.9159567333, 175.4733394833, \"133\"],\n[-37.9133416833, 175.4750525833, \"112\"],\n[-37.9164713167, 175.4731105667, \"139\"],\n[-37.9162964833, 175.4731911, \"137\"],\n[-37.91611805, 175.4732665833, \"135\"],\n[-37.9050444167, 175.4785102333, \"19\"],\n[-37.9084974833, 175.47730975, \"50\"],\n[-37.9168131667, 175.4729816333, \"143\"],\n[-37.9088890667, 175.47665875, \"51\"],\n[-37.9169769833, 175.47291525, \"145\"],\n[-37.91109855, 175.4756779833, \"83\"],\n[-37.9036883833, 175.47891485, \"5\"],\n[-37.9038746333, 175.4790332667, \"7\"],\n[-37.9098547333, 175.4762237333, \"67\"],\n[-37.9134471167, 175.47558925, \"110\"],\n[-37.9091846167, 175.4769739333, \"58\"],\n[-37.9041253333, 175.47834505, \"67\"],\n[-37.9031156667, 175.47324245, \"30\"],\n[-37.9039179333, 175.4771832667, \"57\"],\n[-37.9027583833, 175.4732321833, \"31\"],\n[-37.9039296333, 175.4759913167, \"1/48-5/48\"],\n[-37.9031900333, 175.4735082833, \"32\"],\n[-37.9034018667, 175.4743276167, \"36\"],\n[-37.9023766833, 175.47196845, \"7\"],\n[-37.9032458833, 175.47560575, \"49A\"],\n[-37.9020863667, 175.4722218833, \"9\"],\n[-37.9043371833, 175.4768388333, \"56B\"],\n[-37.902459, 175.47222945, \"15\"],\n[-37.9037351, 175.47533715, \"44\"],\n[-37.9034826333, 175.4756647333, \"51\"],\n[-37.9036591, 175.47507465, \"42\"],\n[-37.90418945, 175.4769539167, \"56\"],\n[-37.9032979333, 175.4750164, \"45\"],\n[-37.9034232167, 175.47544545, \"49\"],\n[-37.9033578, 175.4752311833, \"47\"],\n[-37.9038201667, 175.4756474167, \"46\"],\n[-37.9022973167, 175.4729606, \"25\"],\n[-37.90414875, 175.4768183, \"52\"],\n[-37.90349885, 175.4745345833, \"38\"],\n[-37.9026894, 175.4730117167, \"29\"],\n[-37.9021493667, 175.47246275, \"17\"],\n[-37.9026062167, 175.47272955, \"23\"],\n[-37.9025311667, 175.4725057, \"21\"],\n[-37.9030380667, 175.47297935, \"28\"],\n[-37.9024547167, 175.4734671, \"33\"],\n[-37.9025374333, 175.4713386333, \"2\"],\n[-37.9032365, 175.4747976667, \"43\"],\n[-37.903545, 175.4758786333, \"53\"],\n[-37.9036348667, 175.47615225, \"55\"],\n[-37.9040838167, 175.4777189667, \"61\"],\n[-37.9044308, 175.4778047833, \"62A\"],\n[-37.9045770833, 175.4776408667, \"62\"],\n[-37.9041435833, 175.4779421667, \"63\"],\n[-37.9045395167, 175.4780827833, \"64\"],\n[-37.9042280667, 175.4782127, \"65\"],\n[-37.9046025833, 175.4783325333, \"66\"],\n[-37.9027241, 175.4719824833, \"6\"],\n[-37.9031429167, 175.4744526833, \"41\"],\n[-37.90234395, 175.4730909833, \"27\"],\n[-37.9035807667, 175.474814, \"40\"],\n[-37.9024965667, 175.4735884667, \"35\"],\n[-37.9028375333, 175.4735153, \"37\"],\n[-37.90263765, 175.4716923333, \"4\"],\n[-37.9021975, 175.47257235, \"19\"],\n[-37.9043748667, 175.4775318, \"60\"],\n[-37.90399325, 175.4774612167, \"59\"],\n[-37.9042882333, 175.4772585333, \"58\"],\n[-37.8844172167, 175.4672121, \"84\"],\n[-37.88526725, 175.4677498167, \"89\"],\n[-37.88475025, 175.4713889167, \"129A\"],\n[-37.8850106, 175.4704196167, \"119B\"],\n[-37.8848693167, 175.4653303833, \"67A\"],\n[-37.8849946167, 175.47052095, \"119A\"],\n[-37.8853588667, 175.4594981667, \"21A\"],\n[-37.8840973667, 175.4733293833, \"140\"],\n[-37.88468505, 175.4712750833, \"127B\"],\n[-37.8848600667, 175.47296205, \"141\"],\n[-37.8846001333, 175.4600292833, \"26A\"],\n[-37.8836016167, 175.4733639667, \"142\"],\n[-37.8834011333, 175.4735009333, \"144A\"],\n[-37.8836620833, 175.4735234, \"144\"],\n[-37.8836292333, 175.4677762333, \"92A\"],\n[-37.8848011, 175.4732428833, \"145\"],\n[-37.8848537, 175.4655050667, \"67B\"],\n[-37.8840610333, 175.4737371333, \"146\"],\n[-37.884338, 175.4684939, \"106\"],\n[-37.8844335167, 175.4733986333, \"147\"],\n[-37.88444615, 175.4731180167, \"1/143\"],\n[-37.8840384, 175.4739760167, \"148\"],\n[-37.8846361, 175.4730721, \"2/143\"],\n[-37.8847583667, 175.4735201333, \"149\"],\n[-37.8847493833, 175.4736688167, \"151\"],\n[-37.8846399333, 175.4737421667, \"153A\"],\n[-37.8844193833, 175.4737101333, \"153\"],\n[-37.8844016, 175.4739577167, \"155\"],\n[-37.8846051, 175.4703118167, \"117\"],\n[-37.8842630167, 175.4702309167, \"118\"],\n[-37.8845899833, 175.4706008833, \"119\"],\n[-37.8839889, 175.4702975, \"120\"],\n[-37.8845767833, 175.4708850333, \"121\"],\n[-37.8848939667, 175.471153, \"125\"],\n[-37.8845648, 175.4711997833, \"127A\"],\n[-37.8845397333, 175.4714559833, \"129\"],\n[-37.8845394, 175.4715708167, \"131\"],\n[-37.88370185, 175.4681742833, \"100A\"],\n[-37.8839465, 175.4681139833, \"100\"],\n[-37.8839286333, 175.4682708, \"102\"],\n[-37.8847414667, 175.4683697167, \"101\"],\n[-37.8843311333, 175.4683495167, \"104\"],\n[-37.8850005167, 175.468632, \"105\"],\n[-37.8847132167, 175.4687043167, \"107\"],\n[-37.88432375, 175.4686693, \"108A\"],\n[-37.8841027333, 175.4686521333, \"108B\"],\n[-37.8837668333, 175.4686217667, \"108C\"],\n[-37.8843156167, 175.4690232667, \"110\"],\n[-37.8844019333, 175.4676185833, \"88\"],\n[-37.8839683, 175.4676575, \"90\"],\n[-37.8839636333, 175.4677862833, \"92\"],\n[-37.8847523333, 175.4678389667, \"93\"],\n[-37.8843878667, 175.4678844833, \"94\"],\n[-37.8843912, 175.46807835, \"96\"],\n[-37.88488445, 175.4681411333, \"97A\"],\n[-37.8847461167, 175.4681295167, \"97\"],\n[-37.8850594333, 175.46849965, \"103\"],\n[-37.88521615, 175.46866015, \"105A\"],\n[-37.8852093833, 175.4680692333, \"95B\"],\n[-37.8844562, 175.4666664667, \"82\"],\n[-37.8844693167, 175.4664959833, \"80\"],\n[-37.8850079167, 175.4663639833, \"77\"],\n[-37.8850347667, 175.4662661667, \"75A\"],\n[-37.8845094, 175.46511225, \"70\"],\n[-37.8848360833, 175.46580235, \"71\"],\n[-37.8845048333, 175.4652718333, \"72\"],\n[-37.88484705, 175.4659848667, \"73\"],\n[-37.8846077667, 175.4632926667, \"56\"],\n[-37.8849583, 175.4636067667, \"57\"],\n[-37.8849358667, 175.46389605, \"59\"],\n[-37.8846143167, 175.4635059, \"60\"],\n[-37.8849265833, 175.4640693333, \"61\"],\n[-37.88458485, 175.4638039167, \"62\"],\n[-37.8846887167, 175.4620028833, \"40\"],\n[-37.8846763667, 175.4622161, \"42\"],\n[-37.88439555, 175.46253885, \"50A\"],\n[-37.8843140667, 175.4625245333, \"50B\"],\n[-37.8846659833, 175.4626401167, \"50\"],\n[-37.8843395, 175.4756649667, \"165\"],\n[-37.8843288167, 175.4758768167, \"167\"],\n[-37.8847458167, 175.47598175, \"169\"],\n[-37.8839907333, 175.4754077667, \"154\"],\n[-37.8832283833, 175.4761336167, \"158A\"],\n[-37.8834749667, 175.4760305, \"158\"],\n[-37.8839257333, 175.4761204333, \"160\"],\n[-37.8839105, 175.4763266167, \"162\"],\n[-37.8839068667, 175.4765393833, \"164\"],\n[-37.8839006667, 175.4766556333, \"166A\"],\n[-37.8838300333, 175.4766482833, \"166B\"],\n[-37.8837648667, 175.4766430667, \"166C\"],\n[-37.88370985, 175.4766495333, \"166D\"],\n[-37.88450305, 175.4759053833, \"167A\"],\n[-37.8843063833, 175.47627465, \"173\"],\n[-37.88481905, 175.4593686833, \"20\"],\n[-37.8848109, 175.4595419, \"22\"],\n[-37.8847686333, 175.4602303333, \"28\"],\n[-37.8847849, 175.4600010333, \"26\"],\n[-37.8847695, 175.4604640833, \"30\"],\n[-37.8847524167, 175.4606697667, \"32A\"],\n[-37.8846032167, 175.4607054667, \"32\"],\n[-37.8847437, 175.4609159, \"34\"],\n[-37.8847367167, 175.4610480333, \"36\"],\n[-37.8846226, 175.4597085, \"24A\"],\n[-37.88480055, 175.4597682833, \"24\"],\n[-37.8849988, 175.45680165, \"4\"],\n[-37.88498305, 175.4570515667, \"6\"],\n[-37.8852893167, 175.45737525, \"9A\"],\n[-37.8856279667, 175.4573165333, \"9\"],\n[-37.8852346167, 175.4583441167, \"17\"],\n[-37.8846999, 175.46184155, \"38\"],\n[-37.8842668333, 175.4699965667, \"116\"],\n[-37.8849434, 175.4633167, \"55\"],\n[-37.8849201333, 175.4582107833, \"14\"],\n[-37.8841963333, 175.4680842833, \"98\"],\n[-37.8850626333, 175.4680435333, \"95A\"],\n[-37.8849265167, 175.4579232, \"12\"],\n[-37.8849416333, 175.4576479667, \"10\"],\n[-37.88464885, 175.4628154667, \"52\"],\n[-37.88495495, 175.4630154, \"53\"],\n[-37.8841566, 175.46306945, \"54C\"],\n[-37.8843828833, 175.4631118167, \"54B\"],\n[-37.8846277833, 175.4630559167, \"54A\"],\n[-37.8852737333, 175.4576375667, \"11\"],\n[-37.8849684, 175.4572897333, \"8\"],\n[-37.8842156, 175.4664696, \"80A\"],\n[-37.8852629167, 175.4609307833, \"31A\"],\n[-37.88407215, 175.4664618, \"80B\"],\n[-37.8853292833, 175.4658178167, \"69D\"],\n[-37.8845638833, 175.4761256667, \"171A\"],\n[-37.8843256167, 175.4761031333, \"171\"],\n[-37.88461985, 175.4700835333, \"115\"],\n[-37.8852249667, 175.4591643, \"19\"],\n[-37.8850024667, 175.4655843667, \"69A\"],\n[-37.8853347667, 175.4565859833, \"3\"],\n[-37.8849291167, 175.4710324667, \"123\"],\n[-37.8850487333, 175.46776055, \"91\"],\n[-37.88518005, 175.4657751833, \"69B\"],\n[-37.8847597, 175.4703240833, \"117A\"],\n[-37.8838148667, 175.4733438, \"140A\"],\n[-37.8852627, 175.4581414167, \"15\"],\n[-37.8852686333, 175.4578723833, \"13\"],\n[-37.8843833333, 175.4628765333, \"52A\"],\n[-37.8846312667, 175.47258135, \"135A\"],\n[-37.8841268, 175.4727441333, \"134\"],\n[-37.8841223833, 175.4729087, \"136\"],\n[-37.8844595167, 175.4725233833, \"135\"],\n[-37.8844608, 175.4728356, \"139\"],\n[-37.88411085, 175.4730945167, \"138\"],\n[-37.8844061, 175.4674269333, \"86\"],\n[-37.8846697, 175.4623996833, \"44\"],\n[-37.8843516833, 175.4624326667, \"46\"],\n[-37.8845218833, 175.4609426833, \"34A\"],\n[-37.8845623833, 175.4649360333, \"68\"],\n[-37.8852556667, 175.46580475, \"69C\"],\n[-37.8853163833, 175.4638318667, \"57A\"],\n[-37.8841402833, 175.4725588333, \"132\"],\n[-37.8845058, 175.46627115, \"78\"],\n[-37.88520865, 175.4594427, \"21\"],\n[-37.88518675, 175.4597238, \"23\"],\n[-37.88516405, 175.4602253833, \"27A\"],\n[-37.8851860333, 175.4600092, \"25\"],\n[-37.8851601667, 175.4603894167, \"27B\"],\n[-37.8851518333, 175.4606064167, \"29\"],\n[-37.8851348833, 175.4608605667, \"31\"],\n[-37.8850809, 175.4610533333, \"33\"],\n[-37.8856552, 175.4571406667, \"7A\"],\n[-37.8853045167, 175.4571108167, \"7\"],\n[-37.88540295, 175.4658220667, \"69E\"],\n[-37.8853268167, 175.459619, \"23A\"],\n[-37.8836866, 175.4760956667, \"160A\"],\n[-37.8847632333, 175.4673442, \"83\"],\n[-37.8847711333, 175.4675412167, \"85\"],\n[-37.8848386167, 175.4661800667, \"75\"],\n[-37.8845198, 175.4660549333, \"76\"],\n[-37.8848257667, 175.46637395, \"79\"],\n[-37.88481345, 175.4665570333, \"81\"],\n[-37.8850432333, 175.4640734333, \"61A\"],\n[-37.8854814333, 175.46382035, \"57B\"],\n[-37.8853202, 175.45681275, \"5\"],\n[-37.88467615, 175.4756712, \"165A\"],\n[-37.8843590833, 175.4753791667, \"163\"],\n[-37.8854775, 175.4568292667, \"5A\"],\n[-37.8846230833, 175.4699498, \"113\"],\n[-37.8840125667, 175.47500995, \"150\"],\n[-37.8840018667, 175.4752078667, \"152\"],\n[-37.8846837667, 175.4753169, \"163A\"],\n[-37.8843619333, 175.4750396667, \"161\"],\n[-37.8840304667, 175.4794188333, \"2A\"],\n[-37.8807723833, 175.4785762167, \"38\"],\n[-37.8832733667, 175.4785025167, \"14\"],\n[-37.8805087833, 175.4795802, \"39\"],\n[-37.8825106167, 175.4792898667, \"22\"],\n[-37.8807048667, 175.4788063333, \"40\"],\n[-37.88040735, 175.47993645, \"41\"],\n[-37.8802707, 175.4795716333, \"43\"],\n[-37.8806401833, 175.4791473833, \"46\"],\n[-37.8805187667, 175.4791179667, \"48\"],\n[-37.8803874167, 175.47910085, \"52A\"],\n[-37.8832278833, 175.4793638667, \"16\"],\n[-37.8830907167, 175.4793476833, \"18\"],\n[-37.8821391833, 175.4796965667, \"27\"],\n[-37.8822759, 175.4797246333, \"25\"],\n[-37.88156645, 175.4792199667, \"26\"],\n[-37.88154905, 175.4796941667, \"29\"],\n[-37.8814046333, 175.4791842167, \"28\"],\n[-37.8841534, 175.4798770333, \"1\"],\n[-37.88124075, 175.47917795, \"30\"],\n[-37.8811993833, 175.4787023, \"32\"],\n[-37.8809580167, 175.4791391833, \"34\"],\n[-37.8809750667, 175.4788466833, \"34A\"],\n[-37.8835383667, 175.4798388, \"9\"],\n[-37.8831311167, 175.4801495667, \"15\"],\n[-37.88322745, 175.47980485, \"13\"],\n[-37.8806638833, 175.4795858667, \"37\"],\n[-37.88084555, 175.4786213833, \"36\"],\n[-37.88399585, 175.4798779833, \"3\"],\n[-37.8839041667, 175.4794138833, \"2\"],\n[-37.8837673333, 175.4794031833, \"4\"],\n[-37.8837132667, 175.4798593833, \"7\"],\n[-37.8808555833, 175.47961625, \"35\"],\n[-37.8835850167, 175.47938365, \"8\"],\n[-37.8838495167, 175.4798612, \"5\"],\n[-37.8837031833, 175.4801284167, \"7A\"],\n[-37.8836938167, 175.47892395, \"6\"],\n[-37.8833948833, 175.4788199167, \"12\"],\n[-37.8832576167, 175.4787911, \"14A\"],\n[-37.8834012333, 175.4798226333, \"11\"],\n[-37.8834290167, 175.4793790167, \"10\"],\n[-37.9152153667, 175.5551235833, \"3/5\"],\n[-37.91650135, 175.5636650667, \"82\"],\n[-37.9161045333, 175.5591854333, \"41\"],\n[-37.9154124667, 175.5548626, \"2/5\"],\n[-37.9166862333, 175.55784235, \"32\"],\n[-37.91602365, 175.5560715, \"21\"],\n[-37.91628925, 175.5653876833, \"94\"],\n[-37.9160243833, 175.55493095, \"1/5\"],\n[-37.91517585, 175.5660481167, \"109\"],\n[-37.91440645, 175.5544777167, \"9\"],\n[-37.9154713667, 175.56556335, \"105\"],\n[-37.9123841333, 175.5769109, \"1/239\"],\n[-37.9138841833, 175.5671444, \"127\"],\n[-37.916035, 175.5569371167, \"25\"],\n[-37.91497065, 175.5671545333, \"1/114\"],\n[-37.9146378167, 175.5673035, \"2/114\"],\n[-37.9176808667, 175.5727210167, \"192\"],\n[-37.91296825, 175.57312245, \"1/192\"],\n[-37.9165331, 175.56259155, \"70\"],\n[-37.9128700167, 175.5676242167, \"135\"],\n[-37.9127302333, 175.5677905167, \"137\"],\n[-37.9138889, 175.57603805, \"212\"],\n[-37.8834204167, 175.4653173333, \"2/67\"],\n[-37.8829550167, 175.4680045, \"93A\"],\n[-37.8815600333, 175.4786609667, \"164C\"],\n[-37.8830218167, 175.4623102, \"41\"],\n[-37.8817898333, 175.47891525, \"166A\"],\n[-37.8827127167, 175.46177765, \"40\"],\n[-37.8822979167, 175.4663795167, \"74A\"],\n[-37.8828493167, 175.4591475, \"16\"],\n[-37.88142475, 175.4786348833, \"164D\"],\n[-37.88306005, 175.4616891167, \"37\"],\n[-37.8820609167, 175.4740814833, \"122\"],\n[-37.8826992167, 175.4619383167, \"42\"],\n[-37.8821519667, 175.4815448333, \"209A\"],\n[-37.883041, 175.4620756167, \"39\"],\n[-37.8832654167, 175.4652926667, \"1/67\"],\n[-37.88272565, 175.4616287, \"38\"],\n[-37.8830076333, 175.4626069, \"43\"],\n[-37.8831357, 175.4601246167, \"27\"],\n[-37.8817307, 175.4761582, \"146A\"],\n[-37.8831685833, 175.4596908, \"25\"],\n[-37.8815906167, 175.4788964667, \"166B\"],\n[-37.88278065, 175.4602865167, \"26\"],\n[-37.8835589833, 175.4653321833, \"3/67\"],\n[-37.8825282667, 175.4646378, \"60\"],\n[-37.8813641333, 175.4755528333, \"136A\"],\n[-37.8823392167, 175.4748610333, \"155\"],\n[-37.8833277833, 175.46261325, \"43A\"],\n[-37.8815676833, 175.4731547, \"114B\"],\n[-37.8819471167, 175.4816728, \"211\"],\n[-37.8829108333, 175.4730159, \"139\"],\n[-37.8831197, 175.4654452, \"69A\"],\n[-37.8827723, 175.4605280167, \"28\"],\n[-37.8829114, 175.4647714833, \"59\"],\n[-37.88250115, 175.4649823, \"64\"],\n[-37.8829076, 175.46497065, \"61\"],\n[-37.88293875, 175.4727218, \"135\"],\n[-37.8824690167, 175.4726854833, \"133\"],\n[-37.88310945, 175.4604659833, \"31\"],\n[-37.8831059, 175.4608415833, \"35\"],\n[-37.8827654333, 175.4607287333, \"30\"],\n[-37.8831227667, 175.4602889167, \"29\"],\n[-37.8825220833, 175.4621568833, \"44A\"],\n[-37.88299765, 175.4628680167, \"45\"],\n[-37.8832516833, 175.4628885167, \"45A\"],\n[-37.8826944667, 175.46216875, \"44\"],\n[-37.8825202167, 175.46254835, \"48A\"],\n[-37.8833442667, 175.45699385, \"5\"],\n[-37.8829708667, 175.4632570667, \"51\"],\n[-37.8824811, 175.4725087833, \"131\"],\n[-37.8829959833, 175.4630565833, \"49\"],\n[-37.8826675833, 175.4625577, \"48\"],\n[-37.88331805, 175.45745235, \"9\"],\n[-37.8824531333, 175.47297485, \"141\"],\n[-37.8827041833, 175.45734235, \"8\"],\n[-37.8833336167, 175.4572248667, \"7\"],\n[-37.8829612, 175.4728780167, \"137\"],\n[-37.8823318333, 175.4686371167, \"86\"],\n[-37.8824054833, 175.4672578333, \"78\"],\n[-37.8823379667, 175.4683863333, \"84A-84D\"],\n[-37.8823962167, 175.4674496167, \"80\"],\n[-37.8824414833, 175.46633525, \"74\"],\n[-37.8827537667, 175.4672953, \"85\"],\n[-37.8822319333, 175.4661475333, \"72\"],\n[-37.8824802, 175.4658691833, \"70\"],\n[-37.8827660833, 175.4671242167, \"83\"],\n[-37.8823372667, 175.47505315, \"157\"],\n[-37.8826545167, 175.4751218167, \"157A\"],\n[-37.8820193333, 175.4750065167, \"128\"],\n[-37.8818626167, 175.4777718333, \"152\"],\n[-37.8819736, 175.47592125, \"140\"],\n[-37.8819698, 175.4757241667, \"138\"],\n[-37.8823149, 175.4754510833, \"165\"],\n[-37.8823264833, 175.4752907833, \"163\"],\n[-37.8827581167, 175.4752805833, \"161\"],\n[-37.8829257833, 175.4751779333, \"159\"],\n[-37.8822762, 175.4760006333, \"167\"],\n[-37.8821834, 175.4777217333, \"177\"],\n[-37.8822236833, 175.4803508667, \"199\"],\n[-37.8815487833, 175.4799515, \"172A\"],\n[-37.8821390333, 175.4785808167, \"185\"],\n[-37.8821657167, 175.4779489667, \"179\"],\n[-37.8821615, 175.47814015, \"181\"],\n[-37.8821482167, 175.4783480833, \"183\"],\n[-37.88205545, 175.4801785, \"197\"],\n[-37.8817139833, 175.48044315, \"176\"],\n[-37.8817403667, 175.4799308667, \"172\"],\n[-37.8817075, 175.4806324333, \"176A\"],\n[-37.8816647167, 175.4814856333, \"184\"],\n[-37.8817027333, 175.4808254167, \"178\"],\n[-37.8816651167, 175.48128865, \"182\"],\n[-37.8816775833, 175.4810764167, \"180\"],\n[-37.8821253667, 175.4788135833, \"187\"],\n[-37.88211265, 175.4790205167, \"189\"],\n[-37.88210465, 175.4792012833, \"191\"],\n[-37.8820355, 175.4804062667, \"201\"],\n[-37.88222755, 175.4806704167, \"203A\"],\n[-37.8820247167, 175.4806337333, \"203\"],\n[-37.8821983833, 175.4809817, \"205A\"],\n[-37.8820063, 175.4808974, \"205\"],\n[-37.8819872, 175.4811870167, \"207\"],\n[-37.88197265, 175.4814592667, \"209\"],\n[-37.8831150167, 175.4606360833, \"33\"],\n[-37.8827381667, 175.4614688333, \"36\"],\n[-37.88168275, 175.4731685667, \"114A\"],\n[-37.8825469333, 175.4624433, \"46A\"],\n[-37.8827017167, 175.4623766, \"46\"],\n[-37.8828950667, 175.4652071, \"65\"],\n[-37.88223365, 175.4708144, \"98\"],\n[-37.8826632333, 175.46886835, \"99\"],\n[-37.88294545, 175.4574472333, \"10\"],\n[-37.8820008, 175.47545635, \"132\"],\n[-37.8815931167, 175.4754940333, \"134\"],\n[-37.8815807333, 175.4756172833, \"136\"],\n[-37.8820043167, 175.4752607833, \"130\"],\n[-37.88218895, 175.4774782333, \"175\"],\n[-37.88172165, 175.4801884, \"174\"],\n[-37.8830567667, 175.4618380833, \"37A\"],\n[-37.88330235, 175.4576792167, \"11\"],\n[-37.8818654333, 175.4775383667, \"150\"],\n[-37.8826391333, 175.4630505, \"50C\"],\n[-37.8826306167, 175.4631781333, \"50D\"],\n[-37.8822323333, 175.46476475, \"62\"],\n[-37.8818249833, 175.4749363833, \"128A\"],\n[-37.8826051333, 175.4701299167, \"109\"],\n[-37.8822580667, 175.47622145, \"169\"],\n[-37.8821165333, 175.4729660833, \"112\"],\n[-37.8826087833, 175.4699331333, \"107\"],\n[-37.8819629, 175.4761853, \"146\"],\n[-37.8820175833, 175.4748403667, \"126\"],\n[-37.8825933333, 175.4704919167, \"113\"],\n[-37.8825976, 175.4703054667, \"111\"],\n[-37.8815316833, 175.4759537667, \"142\"],\n[-37.8815485667, 175.4760951167, \"144\"],\n[-37.8826667167, 175.4685696833, \"97A\"],\n[-37.8823006, 175.4698174167, \"90\"],\n[-37.8822800167, 175.4702989667, \"96\"],\n[-37.8819513333, 175.47633525, \"148\"],\n[-37.8828277833, 175.4687268667, \"97\"],\n[-37.8822974333, 175.469983, \"92\"],\n[-37.8828276667, 175.4593929667, \"18\"],\n[-37.8823263167, 175.4688898, \"88\"],\n[-37.8832137, 175.4589384833, \"17\"],\n[-37.8822883667, 175.4701519667, \"94\"],\n[-37.8826732, 175.4684589667, \"97B\"],\n[-37.8822189833, 175.47111545, \"100\"],\n[-37.8820588, 175.4799191833, \"195\"],\n[-37.8817857167, 175.4791120667, \"168\"],\n[-37.8815184167, 175.4779607667, \"156\"],\n[-37.8831976333, 175.4593128167, \"21\"],\n[-37.8823417833, 175.4681742167, \"82\"],\n[-37.8827111, 175.4679744667, \"93\"],\n[-37.8832019667, 175.4591213833, \"19\"],\n[-37.8815556333, 175.4802543833, \"174A\"],\n[-37.88215085, 175.4723449167, \"108\"],\n[-37.8815176167, 175.4778254833, \"154\"],\n[-37.8820986333, 175.4731788833, \"114\"],\n[-37.8820722833, 175.47386775, \"120\"],\n[-37.8821363667, 175.4725972667, \"110\"],\n[-37.8818740833, 175.47394495, \"120A\"],\n[-37.8820951167, 175.4734228167, \"116\"],\n[-37.88208125, 175.4736554333, \"118\"],\n[-37.8816796667, 175.4786736667, \"164B\"],\n[-37.88313585, 175.4678512667, \"91\"],\n[-37.8824046833, 175.4740707, \"153\"],\n[-37.8829457667, 175.45767015, \"12A\"],\n[-37.8825498667, 175.45756275, \"12\"],\n[-37.8832922167, 175.4578983333, \"13\"],\n[-37.8829356167, 175.4578815, \"14\"],\n[-37.8832731833, 175.4580626167, \"15\"],\n[-37.8833720833, 175.4564383833, \"1\"],\n[-37.8833604167, 175.4567315167, \"3\"],\n[-37.8828452833, 175.4659509, \"75\"],\n[-37.88283615, 175.4661431333, \"77\"],\n[-37.8828280833, 175.4662871833, \"79\"],\n[-37.8831762333, 175.4650671667, \"63A\"],\n[-37.8832807333, 175.4650862667, \"63B\"],\n[-37.8828784, 175.4654325333, \"69\"],\n[-37.88286845, 175.4656069833, \"71\"],\n[-37.8828541167, 175.4657729833, \"73\"],\n[-37.88225265, 175.4763298333, \"171\"],\n[-37.88244025, 175.4731903833, \"143\"],\n[-37.88277745, 175.4714596167, \"123A\"],\n[-37.8825218833, 175.4714168, \"123\"],\n[-37.8826166167, 175.4697419333, \"105\"],\n[-37.88317465, 175.4595099667, \"23\"],\n[-37.88146045, 175.47846145, \"162\"],\n[-37.8818026833, 175.47869275, \"164A\"],\n[-37.8825380333, 175.47125165, \"121\"],\n[-37.8821686667, 175.4812678167, \"207A\"],\n[-37.8825423167, 175.47107055, \"119\"],\n[-37.8818212833, 175.4783825, \"160\"],\n[-37.8825595667, 175.4708636, \"117\"],\n[-37.8822055167, 175.4713504167, \"104\"],\n[-37.8821969167, 175.4715105167, \"106\"],\n[-37.88270855, 175.4681556333, \"95\"],\n[-37.8818339, 175.4781130167, \"158\"],\n[-37.88248225, 175.4723453, \"129\"],\n[-37.8825806, 175.4706828667, \"115\"],\n[-37.8824171833, 175.47368635, \"149\"],\n[-37.8830100833, 175.4662410333, \"77A\"],\n[-37.88294835, 175.4681855, \"95A\"],\n[-37.8824262667, 175.4734592, \"147\"],\n[-37.88256465, 175.4739109667, \"151A\"],\n[-37.88265405, 175.4627631667, \"50A\"],\n[-37.88273475, 175.4676983, \"89\"],\n[-37.8826450667, 175.4629121333, \"50B\"],\n[-37.8827506167, 175.4674962833, \"87\"],\n[-37.8824087, 175.4738996167, \"151\"],\n[-37.8752754167, 175.4687945, \"5\"],\n[-37.8762403, 175.4685664667, \"17\"],\n[-37.8754141333, 175.46920485, \"7\"],\n[-37.87579005, 175.4687529833, \"11B\"],\n[-37.8756581167, 175.4691656167, \"9\"],\n[-37.8765213333, 175.4682912, \"19D\"],\n[-37.8756935167, 175.46956425, \"10\"],\n[-37.87642715, 175.46787445, \"19B\"],\n[-37.8755152333, 175.4695951833, \"8\"],\n[-37.8764289167, 175.46891995, \"20\"],\n[-37.8753332833, 175.4696019833, \"6\"],\n[-37.8751625833, 175.4692038167, \"3\"],\n[-37.8751427, 175.4696164667, \"4\"],\n[-37.8764386833, 175.46867495, \"21\"],\n[-37.87496015, 175.4696499, \"2\"],\n[-37.8756744167, 175.4688181667, \"11A\"],\n[-37.8749777333, 175.4692451667, \"1\"],\n[-37.8763775667, 175.4681980333, \"19A\"],\n[-37.87626605, 175.46911145, \"16\"],\n[-37.8765451333, 175.4678920167, \"19C\"],\n[-37.87614825, 175.4692725667, \"14\"],\n[-37.87655055, 175.4693377, \"18\"],\n[-37.8760211, 175.4693800167, \"12\"],\n[-37.87610675, 175.46867635, \"15\"],\n[-37.8759566833, 175.4689563167, \"13\"],\n[-37.8746548333, 175.4923337833, \"108\"],\n[-37.8761834667, 175.4925171833, \"86\"],\n[-37.8786149333, 175.4916796167, \"61\"],\n[-37.8767351, 175.4913667333, \"83\"],\n[-37.8802232333, 175.49233165, \"48\"],\n[-37.8775048833, 175.4913709, \"75\"],\n[-37.8754266, 175.4923112333, \"98\"],\n[-37.8778063333, 175.4924615667, \"72\"],\n[-37.8803238667, 175.49186235, \"45\"],\n[-37.8723323333, 175.49117315, \"131\"],\n[-37.8718419, 175.4924310167, \"138\"],\n[-37.8711135, 175.4946030167, \"148\"],\n[-37.8697698833, 175.4923552333, \"166\"],\n[-37.8688831, 175.4922950167, \"176\"],\n[-37.8789681333, 175.4925074, \"58\"],\n[-37.8820738333, 175.4926333167, \"30\"],\n[-37.8812172, 175.49178955, \"37\"],\n[-37.88069745, 175.4925708333, \"42\"],\n[-37.8787213167, 175.4903820667, \"63\"],\n[-37.8789726667, 175.4909397833, \"59\"],\n[-37.8796000667, 175.4917581167, \"53\"],\n[-37.8795261833, 175.49247645, \"54\"],\n[-37.8736368167, 175.4923986833, \"124\"],\n[-37.8727232333, 175.4924140333, \"126\"],\n[-37.87437165, 175.4674392167, \"27\"],\n[-37.8746732167, 175.4701863, \"7\"],\n[-37.8744574833, 175.4691624667, \"17\"],\n[-37.8743822167, 175.47024785, \"9A\"],\n[-37.8749997667, 175.4700487, \"12\"],\n[-37.8743498667, 175.4700893333, \"9B\"],\n[-37.8752501333, 175.4707564333, \"4\"],\n[-37.8746036167, 175.46989845, \"11\"],\n[-37.8744127333, 175.4663752333, \"33\"],\n[-37.87455375, 175.4696726333, \"13\"],\n[-37.87442195, 175.4683011333, \"21\"],\n[-37.87475005, 175.4704344167, \"5\"],\n[-37.8747738, 175.4664531833, \"32\"],\n[-37.8747376833, 175.4677991, \"24\"],\n[-37.8748038833, 175.4662112833, \"34\"],\n[-37.8743485, 175.4672374833, \"29\"],\n[-37.87475315, 175.4683317, \"22\"],\n[-37.8747034167, 175.4672897833, \"28\"],\n[-37.8751725667, 175.4705402833, \"6\"],\n[-37.8743703, 175.46656865, \"31\"],\n[-37.8747061667, 175.4675099333, \"26\"],\n[-37.8749663833, 175.4711516, \"1\"],\n[-37.8750729167, 175.4702726833, \"10\"],\n[-37.8747800333, 175.4687326833, \"20\"],\n[-37.8747974333, 175.468992, \"18\"],\n[-37.87484445, 175.4707471667, \"3\"],\n[-37.8754537333, 175.47035305, \"8A\"],\n[-37.8753716, 175.4710875, \"2\"],\n[-37.8749196, 175.4697604667, \"14\"],\n[-37.8744452, 175.4661277667, \"35\"],\n[-37.8744961833, 175.46941455, \"15\"],\n[-37.8747449667, 175.4666386167, \"30\"],\n[-37.87482605, 175.4692223833, \"16\"],\n[-37.8754105667, 175.4702048167, \"8B\"],\n[-37.8743849167, 175.4676931, \"25\"],\n[-37.8744337, 175.4689042667, \"19\"],\n[-37.8743984167, 175.4679389167, \"23\"],\n[-37.8778069667, 175.4712312167, \"25\"],\n[-37.8763120333, 175.4717215833, \"44\"],\n[-37.87672545, 175.4712506833, \"41\"],\n[-37.8772550667, 175.4717175167, \"32\"],\n[-37.8668511333, 175.4718110667, \"166\"],\n[-37.8767613667, 175.4717327167, \"40\"],\n[-37.8733181333, 175.47092935, \"81\"],\n[-37.86564525, 175.47179185, \"172\"],\n[-37.866566, 175.47182225, \"170\"],\n[-37.8709313833, 175.472052, \"102\"],\n[-37.8773583833, 175.4709902333, \"33A\"],\n[-37.8718238, 175.4719052833, \"100\"],\n[-37.8636636, 175.4713474333, \"185\"],\n[-37.8639432, 175.4718744833, \"180\"],\n[-37.87639895, 175.4712276333, \"43\"],\n[-37.86320725, 175.47180795, \"190\"],\n[-37.87736335, 175.4707706667, \"33B\"],\n[-37.8635744667, 175.4721996167, \"1/186\"],\n[-37.8770291667, 175.4712897833, \"37\"],\n[-37.8744578833, 175.4712959833, \"63\"],\n[-37.87760745, 175.4717249167, \"30\"],\n[-37.8748192, 175.47130365, \"59\"],\n[-37.8647013333, 175.4717920833, \"174\"],\n[-37.8754668167, 175.4712709333, \"55\"],\n[-37.8748770833, 175.4717543167, \"62\"],\n[-37.863631, 175.4717906, \"186\"],\n[-37.8759205833, 175.4712616667, \"51\"],\n[-37.86283285, 175.4713374833, \"195\"],\n[-37.8773474667, 175.4713414333, \"35\"],\n[-37.87610145, 175.4712462667, \"47\"],\n[-37.8736575, 175.4709245667, \"75\"],\n[-37.8743432333, 175.4718551833, \"66\"],\n[-37.86451155, 175.47179665, \"176\"],\n[-37.8735859667, 175.4719257333, \"74\"],\n[-37.8734058167, 175.47132365, \"79\"],\n[-37.8742359, 175.4712991333, \"67\"],\n[-37.87358325, 175.4713242167, \"77\"],\n[-37.87305765, 175.4719396833, \"82\"],\n[-37.8727949333, 175.4719115167, \"86\"],\n[-37.8722028667, 175.4741686, \"90\"],\n[-37.8675567167, 175.4711739833, \"137\"],\n[-37.8725890333, 175.46970215, \"2/95\"],\n[-37.8726248333, 175.4710150833, \"1/95\"],\n[-37.8702623, 175.4720475333, \"116\"],\n[-37.8780759667, 175.4717211333, \"24\"],\n[-37.8738727167, 175.4713038, \"73\"],\n[-37.8739672333, 175.4710486167, \"71\"],\n[-37.8935260167, 175.4748064, \"10\"],\n[-37.89273555, 175.4747373, \"16\"],\n[-37.8919345833, 175.4746388833, \"24\"],\n[-37.89328595, 175.4747852667, \"12\"],\n[-37.8938477333, 175.47430505, \"8A\"],\n[-37.8936160833, 175.47481635, \"8\"],\n[-37.8920694833, 175.4757293833, \"23A\"],\n[-37.89206575, 175.4752388667, \"23\"],\n[-37.8923724, 175.4746924667, \"18\"],\n[-37.89353315, 175.47444375, \"10A\"],\n[-37.8899805167, 175.4750166667, \"41\"],\n[-37.89167325, 175.4757043333, \"27A\"],\n[-37.89028415, 175.47505045, \"39\"],\n[-37.892463, 175.47526755, \"17\"],\n[-37.8918398667, 175.4752066, \"25\"],\n[-37.8908128167, 175.4750707333, \"33\"],\n[-37.8904636333, 175.4750504333, \"37\"],\n[-37.8906359833, 175.47507655, \"35\"],\n[-37.8916253, 175.47516515, \"27\"],\n[-37.8909492333, 175.47507265, \"31\"],\n[-37.8905761667, 175.4744542167, \"1/30-5/30\"],\n[-37.8881462333, 175.4754198167, \"57\"],\n[-37.88850555, 175.47524575, \"53\"],\n[-37.8894041667, 175.47498115, \"45\"],\n[-37.9014205, 175.4676039667, \"9\"],\n[-37.9052175667, 175.4691138833, \"53\"],\n[-37.90503895, 175.46905685, \"51\"],\n[-37.90533595, 175.4686957333, \"55\"],\n[-37.90163855, 175.46769255, \"17\"],\n[-37.9033958, 175.4683840167, \"35\"],\n[-37.9053689667, 175.46858075, \"57\"],\n[-37.9012599667, 175.4679458667, \"12\"],\n[-37.9014051333, 175.46801755, \"14\"],\n[-37.9015679, 175.4680723333, \"16\"],\n[-37.9031653, 175.4679986833, \"31A\"],\n[-37.9007696333, 175.46734585, \"1\"],\n[-37.9017356333, 175.4681344333, \"18\"],\n[-37.9019312333, 175.46780625, \"19\"],\n[-37.9019026, 175.4682113333, \"20\"],\n[-37.9023515833, 175.4679880667, \"23\"],\n[-37.9025247833, 175.4680511, \"25\"],\n[-37.9019499, 175.46872425, \"26\"],\n[-37.9027011167, 175.4680964167, \"27\"],\n[-37.90322595, 175.4683034667, \"33\"],\n[-37.9022090667, 175.46834225, \"28\"],\n[-37.9028740333, 175.4681630167, \"29\"],\n[-37.9009473167, 175.4678397833, \"2\"],\n[-37.9030474, 175.46823325, \"31\"],\n[-37.9024255333, 175.4684154833, \"32\"],\n[-37.9024751, 175.4690537, \"34A\"],\n[-37.9025607, 175.4684754, \"34\"],\n[-37.9047760167, 175.4688679333, \"49\"],\n[-37.9035690833, 175.46844705, \"37\"],\n[-37.9032248, 175.4687414833, \"38\"],\n[-37.9037398833, 175.4685398, \"39\"],\n[-37.90425835, 175.4687235833, \"45\"],\n[-37.9039185, 175.4686071, \"41\"],\n[-37.9040853333, 175.4686607667, \"43\"],\n[-37.9044377833, 175.46879205, \"47\"],\n[-37.9027932167, 175.4685815833, \"36\"],\n[-37.90099425, 175.46709335, \"3\"],\n[-37.90111195, 175.4678951333, \"4\"],\n[-37.9010127333, 175.4674434667, \"5\"],\n[-37.9012417333, 175.4675368333, \"7\"],\n[-37.9054492167, 175.4684984667, \"59\"],\n[-37.9055391, 175.4685278333, \"61\"],\n[-37.90550535, 175.46878055, \"63\"],\n[-37.9054566667, 175.46921805, \"65\"],\n[-37.90230745, 175.4676206667, \"21A\"],\n[-37.90216175, 175.4679036667, \"21\"],\n[-37.8808262333, 175.4773818, \"55A\"],\n[-37.8830005833, 175.4767448167, \"16\"],\n[-37.8828296667, 175.4767286, \"18\"],\n[-37.88259875, 175.4772334333, \"31\"],\n[-37.8824754, 175.47721195, \"33\"],\n[-37.8831398167, 175.4779190333, \"17\"],\n[-37.8825963167, 175.4777829, \"29\"],\n[-37.8842646833, 175.4768462333, \"2\"],\n[-37.8810851333, 175.476127, \"36\"],\n[-37.8806964, 175.47760075, \"57\"],\n[-37.8805952167, 175.4775984333, \"59\"],\n[-37.8813687667, 175.4765958833, \"32\"],\n[-37.8824115167, 175.4772049833, \"35\"],\n[-37.8811797, 175.4765765833, \"34\"],\n[-37.8815471667, 175.4766046333, \"30\"],\n[-37.88417235, 175.4773933667, \"3\"],\n[-37.88174695, 175.4772075667, \"39\"],\n[-37.8809885833, 175.47654575, \"38\"],\n[-37.8814635333, 175.4771838167, \"43\"],\n[-37.8816058667, 175.4771958, \"41\"],\n[-37.88064845, 175.4765253, \"42\"],\n[-37.8808178333, 175.4765337333, \"40\"],\n[-37.883982, 175.4773589667, \"5\"],\n[-37.8834955667, 175.4767355667, \"10\"],\n[-37.88350755, 175.4765409, \"10A\"],\n[-37.8836987667, 175.47736385, \"7\"],\n[-37.88321935, 175.4767625833, \"12\"],\n[-37.8833260667, 175.4773372, \"13\"],\n[-37.8830089, 175.4772931833, \"19A\"],\n[-37.8831393167, 175.4773106833, \"19\"],\n[-37.88264695, 175.47671705, \"20\"],\n[-37.88292945, 175.4775952333, \"21\"],\n[-37.8824614333, 175.4767002833, \"22\"],\n[-37.8828817167, 175.4779039, \"23\"],\n[-37.8823059167, 175.4766897833, \"24\"],\n[-37.8827997667, 175.4772721833, \"25A\"],\n[-37.8828034167, 175.4775536833, \"25B\"],\n[-37.8827016667, 175.4777941, \"27\"],\n[-37.8828062, 175.47779805, \"27A\"],\n[-37.8817341833, 175.4766062333, \"28\"],\n[-37.8813136167, 175.4771594167, \"45\"],\n[-37.8811630667, 175.4771457, \"47\"],\n[-37.8809942833, 175.4776755833, \"51\"],\n[-37.8804464667, 175.4764969833, \"44\"],\n[-37.8810737, 175.4776356667, \"49\"],\n[-37.8809778333, 175.4774265, \"53A\"],\n[-37.8810030333, 175.4771406667, \"53\"],\n[-37.88079105, 175.47712645, \"55\"],\n[-37.8835414333, 175.4773520667, \"9\"],\n[-37.8805955667, 175.4771014667, \"61\"],\n[-37.8804778167, 175.4770884667, \"63\"],\n[-37.8802896833, 175.4770558, \"65\"],\n[-37.8834467833, 175.4778119333, \"11\"],\n[-37.8878083, 175.4623779167, \"49A\"],\n[-37.8872817833, 175.45737585, \"10\"],\n[-37.8877537333, 175.4595533, \"27A\"],\n[-37.8884375167, 175.45748685, \"11B\"],\n[-37.8878958, 175.4596416167, \"27B\"],\n[-37.88824535, 175.45746395, \"11A\"],\n[-37.8875099833, 175.4575973667, \"14\"],\n[-37.8866716167, 175.4603215333, \"36B\"],\n[-37.8871296333, 175.4604902833, \"36C\"],\n[-37.8873681333, 175.4599526667, \"30\"],\n[-37.8880297, 175.4601033833, \"31A\"],\n[-37.8881606833, 175.4601333667, \"31B\"],\n[-37.8869335833, 175.4608386833, \"40A\"],\n[-37.8873096333, 175.4609038, \"40\"],\n[-37.8878578167, 175.4576188333, \"13\"],\n[-37.8871150667, 175.4574956333, \"12\"],\n[-37.8878406333, 175.45785455, \"15\"],\n[-37.8871478667, 175.4578061667, \"16A\"],\n[-37.8869680833, 175.4578044667, \"16B\"],\n[-37.8874965167, 175.4577967, \"16\"],\n[-37.8872507333, 175.4580158333, \"18A\"],\n[-37.88747815, 175.4580365167, \"18\"],\n[-37.8880638833, 175.45810725, \"19A\"],\n[-37.88784345, 175.4580772667, \"19\"],\n[-37.8877572333, 175.45932705, \"25\"],\n[-37.8879145667, 175.45669455, \"1\"],\n[-37.8873702833, 175.4595467333, \"26\"],\n[-37.8879238167, 175.4569262833, \"1A\"],\n[-37.8874736, 175.4582577833, \"20\"],\n[-37.8878260333, 175.4583038167, \"21\"],\n[-37.8873656333, 175.4597425833, \"28\"],\n[-37.8879435667, 175.4597720333, \"29A\"],\n[-37.8881376167, 175.45977945, \"29B\"],\n[-37.8883413667, 175.4597758333, \"29C\"],\n[-37.8884945333, 175.45978905, \"29D\"],\n[-37.8877360167, 175.45978725, \"29\"],\n[-37.8871679167, 175.4598937333, \"30A\"],\n[-37.8877368167, 175.45999715, \"31\"],\n[-37.8873456, 175.4602082167, \"32\"],\n[-37.8877073333, 175.4602192333, \"33\"],\n[-37.88687805, 175.4602032667, \"34A\"],\n[-37.8866636, 175.4602061833, \"34B\"],\n[-37.8868802667, 175.4603641333, \"36A\"],\n[-37.8877013333, 175.4604213, \"35\"],\n[-37.8875414167, 175.4568279333, \"2\"],\n[-37.8873298167, 175.4604643167, \"36\"],\n[-37.8876929833, 175.4606236833, \"37\"],\n[-37.8869503167, 175.4606911333, \"38A\"],\n[-37.8873173833, 175.46064935, \"38\"],\n[-37.8880788167, 175.46095015, \"39A\"],\n[-37.8876923833, 175.4607943167, \"39\"],\n[-37.88767155, 175.4610391833, \"41\"],\n[-37.8876050833, 175.4618428, \"43\"],\n[-37.8876116333, 175.4619894, \"45\"],\n[-37.88820765, 175.4571291333, \"3A\"],\n[-37.88840855, 175.4571648833, \"3B\"],\n[-37.8875992833, 175.4621293167, \"47\"],\n[-37.88757835, 175.4622876333, \"49\"],\n[-37.8875702, 175.4625005667, \"51\"],\n[-37.8875504, 175.46284035, \"53\"],\n[-37.8875351833, 175.46302475, \"55\"],\n[-37.88811905, 175.4635122, \"57E\"],\n[-37.88825965, 175.46351715, \"57F\"],\n[-37.8882625, 175.4633885, \"57G\"],\n[-37.8882589333, 175.4632448167, \"57H\"],\n[-37.8875218833, 175.4632180167, \"57\"],\n[-37.8875030667, 175.4634566667, \"59\"],\n[-37.8871363, 175.4643333667, \"64\"],\n[-37.88744265, 175.4645542333, \"65\"],\n[-37.8871286333, 175.4645791667, \"66\"],\n[-37.8876065667, 175.4648085833, \"67A\"],\n[-37.88744175, 175.4647992167, \"67\"],\n[-37.88711895, 175.4648136, \"68\"],\n[-37.8885710333, 175.4571354, \"5A\"],\n[-37.8886647667, 175.4571676667, \"5B\"],\n[-37.8874510667, 175.4649796333, \"69A\"],\n[-37.8875224833, 175.4649906833, \"69B\"],\n[-37.8878563833, 175.4571620833, \"7A\"],\n[-37.8880328667, 175.4571656833, \"7B\"],\n[-37.8875264167, 175.4573541, \"8\"],\n[-37.88804485, 175.4574312167, \"9B\"],\n[-37.8875345667, 175.4570015833, \"6\"],\n[-37.8878647667, 175.45740435, \"9A\"],\n[-37.8879745833, 175.4634900667, \"57D\"],\n[-37.8878037333, 175.4633596833, \"57B\"],\n[-37.8878387167, 175.4631928333, \"57A\"],\n[-37.8878102333, 175.46349065, \"57C\"],\n[-37.9019538167, 175.4671126833, \"7A\"],\n[-37.903244, 175.4676256333, \"14A\"],\n[-37.9020871667, 175.4668353167, \"7\"],\n[-37.9027623667, 175.4671213167, \"11\"],\n[-37.9025892667, 175.4670505333, \"10\"],\n[-37.9024684667, 175.4672691667, \"10A\"],\n[-37.90241945, 175.4669742667, \"9\"],\n[-37.9029300667, 175.4671840333, \"12\"],\n[-37.90311145, 175.4672579333, \"13\"],\n[-37.9032782833, 175.4673314833, \"14\"],\n[-37.9036218833, 175.4674643167, \"16\"],\n[-37.9037919667, 175.46753785, \"17\"],\n[-37.90344895, 175.4673941667, \"15\"],\n[-37.9039705667, 175.4676045333, \"18\"],\n[-37.9041435833, 175.467671, \"19\"],\n[-37.9010125167, 175.4664210333, \"1A\"],\n[-37.9007809667, 175.4663229167, \"1\"],\n[-37.9043140333, 175.4677303167, \"20\"],\n[-37.90448985, 175.46780755, \"21\"],\n[-37.9046493333, 175.4670530667, \"23\"],\n[-37.9048533833, 175.4675182833, \"24\"],\n[-37.9012388167, 175.46653205, \"2\"],\n[-37.9014534667, 175.46660645, \"4\"],\n[-37.9017320167, 175.4669518333, \"5\"],\n[-37.9018944667, 175.4667655333, \"6\"],\n[-37.9022384667, 175.46689735, \"8\"],\n[-37.90570045, 175.46976825, \"72/91\"],\n[-37.9051381333, 175.4722857333, \"96\"],\n[-37.9032204, 175.4703621333, \"53\"],\n[-37.9053162167, 175.4723414, \"98\"],\n[-37.9037962833, 175.4711215833, \"5/91\"],\n[-37.9055880667, 175.4707876167, \"108/91\"],\n[-37.9045404333, 175.4699855167, \"46/91\"],\n[-37.9016813667, 175.4703439167, \"17\"],\n[-37.9047542667, 175.4700433333, \"48/91\"],\n[-37.9038862333, 175.4703755167, \"31/91\"],\n[-37.9046661667, 175.4695472667, \"56/91\"],\n[-37.9033795167, 175.4706831, \"10/91\"],\n[-37.9049449167, 175.4696617167, \"54/91\"],\n[-37.90342665, 175.4705010167, \"26/91\"],\n[-37.9042283333, 175.4698824, \"43/91\"],\n[-37.9034820167, 175.4702482, \"27/91\"],\n[-37.9041029833, 175.471257, \"3/91\"],\n[-37.9035045167, 175.47071685, \"11/91\"],\n[-37.9034292833, 175.4709543667, \"8/91\"],\n[-37.9035455167, 175.4705136333, \"25/91\"],\n[-37.9044278, 175.4696974167, \"52/91\"],\n[-37.9036199667, 175.4702949333, \"28/91\"],\n[-37.9050604833, 175.4696742167, \"53/91\"],\n[-37.9035777167, 175.4701138, \"29/91\"],\n[-37.9047726833, 175.46981425, \"49/91\"],\n[-37.9037153, 175.4701772333, \"30/91\"],\n[-37.90366135, 175.4710693333, \"6/91\"],\n[-37.9036912333, 175.4708029167, \"12/91\"],\n[-37.9039544333, 175.4701398833, \"39/91\"],\n[-37.90384865, 175.4708670667, \"13/91\"],\n[-37.90441965, 175.4694715667, \"58/91\"],\n[-37.9039537167, 175.4709042667, \"14/91\"],\n[-37.9048014667, 175.46941845, \"66/91\"],\n[-37.90397155, 175.4704078333, \"32/91\"],\n[-37.9046743167, 175.4697731167, \"50/91\"],\n[-37.9040982, 175.4709554167, \"15/91\"],\n[-37.9049822333, 175.4694750833, \"67/91\"],\n[-37.9042064833, 175.4709969, \"16/91\"],\n[-37.9039450167, 175.4711861667, \"4/91\"],\n[-37.9043704167, 175.47106545, \"17/91\"],\n[-37.9045624167, 175.4693054333, \"64/91\"],\n[-37.90449515, 175.4711117167, \"18/91\"],\n[-37.9040047, 175.46996625, \"40/91\"],\n[-37.9038797333, 175.4706635, \"24/91\"],\n[-37.9035451833, 175.4710177667, \"7/91\"],\n[-37.9039780833, 175.4707046333, \"23/91\"],\n[-37.9045066833, 175.46972115, \"51/91\"],\n[-37.9041615833, 175.4707864333, \"22/91\"],\n[-37.9060640333, 175.4699358833, \"91/91\"],\n[-37.9042534333, 175.47082315, \"21/91\"],\n[-37.9047130333, 175.4693776833, \"65/91\"],\n[-37.9044305, 175.47089635, \"20/91\"],\n[-37.9043041167, 175.46989765, \"44/91\"],\n[-37.9041919167, 175.4691500833, \"61/91\"],\n[-37.9044746667, 175.46996645, \"45/91\"],\n[-37.9042904667, 175.4691828667, \"62/91\"],\n[-37.9040487167, 175.4697756833, \"41/91\"],\n[-37.90415055, 175.4693699833, \"60/91\"],\n[-37.90446065, 175.46926835, \"63/91\"],\n[-37.9042810333, 175.46945405, \"59/91\"],\n[-37.9032851, 175.4708958667, \"9/91\"],\n[-37.90451895, 175.4709371333, \"19/91\"],\n[-37.9045116833, 175.4694999333, \"57/91\"],\n[-37.9040854167, 175.4704954667, \"33/91\"],\n[-37.9043916, 175.4713621667, \"1/91\"],\n[-37.9041970167, 175.4705370833, \"34/91\"],\n[-37.9050893833, 175.46956665, \"68/91\"],\n[-37.9043310833, 175.4706087333, \"35/91\"],\n[-37.9040848, 175.4696433167, \"42/91\"],\n[-37.90443305, 175.4706374667, \"36/91\"],\n[-37.90476785, 175.4695885167, \"55/91\"],\n[-37.9045282833, 175.4706701333, \"37/91\"],\n[-37.9042286667, 175.471303, \"2/91\"],\n[-37.90462665, 175.4707112667, \"38/91\"],\n[-37.9046719667, 175.4700236667, \"47/91\"],\n[-37.9063787833, 175.47006835, \"93/91\"],\n[-37.9052918833, 175.4698329167, \"76/91\"],\n[-37.9052114833, 175.4700221833, \"77/91\"],\n[-37.9050883833, 175.4701973667, \"79/91\"],\n[-37.9053839167, 175.4698613, \"75/91\"],\n[-37.9052999333, 175.47006295, \"78/91\"],\n[-37.9051933667, 175.47023875, \"80/91\"],\n[-37.9052919167, 175.4702715333, \"81/91\"],\n[-37.9055609, 175.4699386667, \"74/91\"],\n[-37.9056495333, 175.4699711, \"73/91\"],\n[-37.9054736, 175.4701402167, \"82/91\"],\n[-37.9063660167, 175.4715132167, \"102/91\"],\n[-37.9062675667, 175.4719148667, \"118/91\"],\n[-37.9062345, 175.4720598833, \"134/91\"],\n[-37.9061993167, 175.4712732833, \"104/91\"],\n[-37.9061410333, 175.4715093, \"117/91\"],\n[-37.90608265, 175.4717494667, \"119/91\"],\n[-37.9060274833, 175.47199395, \"133/91\"],\n[-37.9059191, 175.4719566333, \"132/91\"],\n[-37.9059778667, 175.47169975, \"120/91\"],\n[-37.90603625, 175.4714595667, \"116/91\"],\n[-37.9061060667, 175.4711529667, \"105/91\"],\n[-37.9053162667, 175.4716843, \"137/91\"],\n[-37.9055131833, 175.4717582167, \"136/91\"],\n[-37.9055088, 175.471073, \"112/91\"],\n[-37.9056170833, 175.4711144833, \"113/91\"],\n[-37.9057647667, 175.4711699333, \"114/91\"],\n[-37.9058762667, 175.4712157167, \"115/91\"],\n[-37.9054861167, 175.4707589, \"109/91\"],\n[-37.9055522167, 175.4701764667, \"83/91\"],\n[-37.9057089833, 175.4702698167, \"84/91\"],\n[-37.9058203833, 175.4703197833, \"85/91\"],\n[-37.90590455, 175.4701097667, \"89/91\"],\n[-37.9059963, 175.4701506667, \"88/91\"],\n[-37.90573235, 175.4708471167, \"107/91\"],\n[-37.9061820167, 175.4702237333, \"87/91\"],\n[-37.9062864333, 175.4702866167, \"86/91\"],\n[-37.9058177667, 175.47087525, \"106/91\"],\n[-37.9064954833, 175.4701769667, \"94/91\"],\n[-37.9065182667, 175.4703407, \"95/91\"],\n[-37.90647405, 175.4705396167, \"96/91\"],\n[-37.906434, 175.4707011, \"97/91\"],\n[-37.9064003667, 175.47087115, \"98/91\"],\n[-37.9065184833, 175.4709171667, \"100/91\"],\n[-37.9053462, 175.4696260167, \"69/91\"],\n[-37.9054348333, 175.46965845, \"70/91\"],\n[-37.9056151167, 175.4697359333, \"71/91\"],\n[-37.9065477667, 175.4707929167, \"99/91\"],\n[-37.9063361667, 175.4716625167, \"103/91\"],\n[-37.9064024833, 175.47136415, \"101/91\"],\n[-37.9062771167, 175.4700270833, \"92/91\"],\n[-37.90561825, 175.4717954167, \"135/91\"],\n[-37.9052021167, 175.4714629667, \"127/91\"],\n[-37.9053039667, 175.4714958833, \"128/91\"],\n[-37.9055797833, 175.4715935333, \"130/91\"],\n[-37.9057338, 175.4716617333, \"131/91\"],\n[-37.9054388, 175.47153415, \"129/91\"],\n[-37.90521165, 175.4711876167, \"126/91\"],\n[-37.9053168167, 175.47122065, \"125/91\"],\n[-37.9054577833, 175.4712800167, \"124/91\"],\n[-37.9055496333, 175.47131675, \"123/91\"],\n[-37.9057004333, 175.47138065, \"122/91\"],\n[-37.9057989, 175.4714176167, \"121/91\"],\n[-37.9052758833, 175.4709810667, \"110/91\"],\n[-37.9053744333, 175.4710138667, \"111/91\"],\n[-37.9029642833, 175.4708119167, \"49\"],\n[-37.9013200667, 175.47020055, \"9\"],\n[-37.9059721, 175.4699033333, \"90/91\"],\n[-37.9009692833, 175.4700359167, \"1\"],\n[-37.9020093333, 175.4704903167, \"25\"],\n[-37.9028505, 175.4713808167, \"42\"],\n[-37.9026220667, 175.4707023167, \"41\"],\n[-37.9030775167, 175.47144105, \"44\"],\n[-37.9032627, 175.4715188333, \"46\"],\n[-37.90279945, 175.4707535, \"47\"],\n[-37.9034596333, 175.4715854667, \"48\"],\n[-37.9046305667, 175.4720644, \"90\"],\n[-37.9048369667, 175.472151, \"92\"],\n[-37.9049919, 175.4722146, \"94\"],\n[-37.9036494333, 175.4716701167, \"50\"],\n[-37.90358525, 175.4846141, \"12\"],\n[-37.9037579, 175.4852529, \"6\"],\n[-37.9038024667, 175.4850978, \"7\"],\n[-37.9037217667, 175.4849037333, \"8\"],\n[-37.90347035, 175.4850285667, \"2\"],\n[-37.9032276333, 175.48546265, \"3\"],\n[-37.9035196167, 175.4852706167, \"4\"],\n[-37.9036445667, 175.4853146167, \"5\"],\n[-37.893842, 175.4658828667, \"14\"],\n[-37.8942232167, 175.4658869, \"10\"],\n[-37.8942694, 175.4655608, \"10A\"],\n[-37.8939012167, 175.46629055, \"11\"],\n[-37.89414105, 175.46563135, \"12A\"],\n[-37.89403175, 175.4658608667, \"12\"],\n[-37.8928791667, 175.4657609, \"18\"],\n[-37.8920411833, 175.4656866167, \"26\"],\n[-37.8947138333, 175.46562585, \"4A\"],\n[-37.89482635, 175.4656405333, \"4B\"],\n[-37.8943928667, 175.4659067667, \"8\"],\n[-37.89417895, 175.4666384333, \"7A\"],\n[-37.8941652333, 175.4663256, \"7\"],\n[-37.8930164333, 175.4657825333, \"16\"],\n[-37.8945337167, 175.46636655, \"3\"],\n[-37.8947049667, 175.4662976667, \"1\"],\n[-37.8943467167, 175.4663510667, \"5\"],\n[-37.8945939667, 175.46589925, \"6A\"],\n[-37.8946026667, 175.4655432, \"6B\"],\n[-37.89400605, 175.4666357, \"9A\"],\n[-37.89400415, 175.4663002333, \"9\"],\n[-37.8947237, 175.4659225, \"4\"],\n[-37.8948905833, 175.4659464167, \"2\"],\n[-37.8926863, 175.465739, \"20\"],\n[-37.8923385, 175.4654623833, \"24B\"],\n[-37.89249375, 175.4657308833, \"22\"],\n[-37.8923111667, 175.4657147167, \"24\"],\n[-37.8815449167, 175.4655157, \"12\"],\n[-37.8822096833, 175.4655813667, \"2\"],\n[-37.88233595, 175.4651839667, \"1\"],\n[-37.8816413667, 175.4648852333, \"9B\"],\n[-37.88217595, 175.4651827, \"3\"],\n[-37.8816216167, 175.4650913833, \"9A\"],\n[-37.8814350167, 175.4653094667, \"13\"],\n[-37.8813971333, 175.4649169833, \"11B\"],\n[-37.8814120833, 175.4650868167, \"11A\"],\n[-37.8819977833, 175.4651571333, \"5\"],\n[-37.8818037833, 175.46516125, \"7\"],\n[-37.9272509333, 175.5707935167, \"17\"],\n[-37.92487165, 175.5688565167, \"50\"],\n[-37.9279805, 175.5715888167, \"2/4\"],\n[-37.9281482167, 175.57159225, \"3/4\"],\n[-37.9277371667, 175.5716141833, \"1/4\"],\n[-37.9283277667, 175.5715995333, \"4/4\"],\n[-37.9259907667, 175.5697310833, \"41\"],\n[-37.9139028667, 175.4794934333, \"16\"],\n[-37.9131072667, 175.4784122, \"5\"],\n[-37.913859, 175.4793484, \"14\"],\n[-37.9137596833, 175.4789706167, \"10\"],\n[-37.9129783833, 175.4779572167, \"1\"],\n[-37.9134818333, 175.4796230333, \"13\"],\n[-37.9134313, 175.47942965, \"11\"],\n[-37.9130481833, 175.4781893667, \"3\"],\n[-37.9136100333, 175.4785928, \"8\"],\n[-37.9137780667, 175.47816635, \"6A\"],\n[-37.9134069333, 175.4779523167, \"2\"],\n[-37.9134797167, 175.4781646167, \"4\"],\n[-37.9135479833, 175.4783853833, \"6\"],\n[-37.9074968667, 175.4759960667, \"26\"],\n[-37.90790835, 175.4788683333, \"53\"],\n[-37.9077523333, 175.47593355, \"26A\"],\n[-37.9075665667, 175.47624025, \"30\"],\n[-37.9073841167, 175.47837545, \"45\"],\n[-37.90830365, 175.4786741167, \"52\"],\n[-37.9087247333, 175.4801247333, \"64\"],\n[-37.9075075667, 175.4788175333, \"51\"],\n[-37.9088166, 175.4804266333, \"66\"],\n[-37.9082595167, 175.4801578667, \"65\"],\n[-37.9082304, 175.47843325, \"50\"],\n[-37.90781645, 175.4785779, \"49\"],\n[-37.9083352333, 175.48044785, \"67\"],\n[-37.9065845333, 175.4744138667, \"15\"],\n[-37.9069974, 175.47423525, \"12\"],\n[-37.9065571167, 175.4727805333, \"2\"],\n[-37.9066387833, 175.47305845, \"4\"],\n[-37.9068011, 175.4735746167, \"8\"],\n[-37.90669225, 175.47329415, \"6\"],\n[-37.9072826667, 175.4752864833, \"18\"],\n[-37.9081420333, 175.47812825, \"44\"],\n[-37.9076585, 175.47803815, \"43\"],\n[-37.9079773, 175.4776514, \"42\"],\n[-37.9080697, 175.4779212, \"42A\"],\n[-37.90758785, 175.4777916167, \"41\"],\n[-37.90774285, 175.4783192, \"47\"],\n[-37.9085469333, 175.47804035, \"46\"],\n[-37.907853, 175.4771642833, \"40\"],\n[-37.90694245, 175.4762600833, \"27A\"],\n[-37.90714115, 175.4762523167, \"27\"],\n[-37.9085940333, 175.4781931, \"48\"],\n[-37.90842915, 175.4807205833, \"69\"],\n[-37.90889165, 175.4806537667, \"68\"],\n[-37.9090825167, 175.4813461833, \"72\"],\n[-37.9085069833, 175.48098455, \"71\"],\n[-37.9091493333, 175.4815615167, \"74\"],\n[-37.9085867333, 175.4812446833, \"73\"],\n[-37.90638345, 175.4737115833, \"9\"],\n[-37.9073718833, 175.4770443667, \"35\"],\n[-37.9076616333, 175.47651175, \"34\"],\n[-37.9070147167, 175.4757612667, \"19\"],\n[-37.9080529667, 175.4794295333, \"61\"],\n[-37.9081081167, 175.4796110667, \"63\"],\n[-37.9067812333, 175.4766508667, \"29\"],\n[-37.90727425, 175.4767382167, \"33\"],\n[-37.9080598, 175.4763364, \"32\"],\n[-37.9080013, 175.4761604, \"32A\"],\n[-37.9072168667, 175.4765151833, \"31\"],\n[-37.9069078, 175.47611375, \"21A\"],\n[-37.9070925167, 175.4759961167, \"21\"],\n[-37.9073595833, 175.4755532, \"20\"],\n[-37.9078311, 175.47552015, \"22\"],\n[-37.9066715333, 175.4762365333, \"23\"],\n[-37.9074292167, 175.4757909, \"24\"],\n[-37.9067200667, 175.4763614333, \"25\"],\n[-37.9079507667, 175.47594195, \"28\"],\n[-37.9077839333, 175.4769487333, \"38\"],\n[-37.9087032167, 175.478581, \"54\"],\n[-37.90755175, 175.4791360667, \"55\"],\n[-37.9087507167, 175.47871815, \"56\"],\n[-37.9075607333, 175.4792694833, \"57\"],\n[-37.9085885667, 175.4789704, \"58A\"],\n[-37.908387, 175.47897535, \"58\"],\n[-37.9078259667, 175.4793328833, \"59A\"],\n[-37.90801855, 175.47915195, \"59\"],\n[-37.90845065, 175.4792147, \"60\"],\n[-37.9086727, 175.4815190333, \"75\"],\n[-37.9092022333, 175.4817325833, \"76\"],\n[-37.9087436333, 175.4817835, \"77\"],\n[-37.9068335333, 175.475831, \"19A\"],\n[-37.9064835, 175.4740658167, \"11\"],\n[-37.9068924667, 175.4738985333, \"10\"],\n[-37.90772245, 175.47673845, \"36\"],\n[-37.9153057833, 175.4700439333, \"8\"],\n[-37.9151920167, 175.470103, \"7\"],\n[-37.9154897833, 175.4704254667, \"4\"],\n[-37.91544255, 175.4701449833, \"6\"],\n[-37.9152406333, 175.4705098833, \"3\"],\n[-37.9155660833, 175.47067085, \"2\"],\n[-37.9153145667, 175.470762, \"1\"],\n[-37.915152, 175.4703036833, \"5\"],\n[-37.8753657, 175.4666292833, \"7\"],\n[-37.8752355833, 175.4664042833, \"5\"],\n[-37.8751239167, 175.4667343, \"3\"],\n[-37.8749005333, 175.4667724833, \"1\"],\n[-37.8759491, 175.4666146833, \"13\"],\n[-37.8761423333, 175.4666642667, \"15\"],\n[-37.8757443667, 175.46657535, \"11\"],\n[-37.8755500833, 175.4665719333, \"9\"],\n[-37.8762191, 175.4673812833, \"18\"],\n[-37.8764603333, 175.4671767333, \"22\"],\n[-37.87631595, 175.4667095833, \"17\"],\n[-37.8765421, 175.4670481833, \"23\"],\n[-37.8764974333, 175.4666557, \"19\"],\n[-37.8765351667, 175.46685605, \"21\"],\n[-37.8763332, 175.4671934833, \"20\"],\n[-37.8761136667, 175.4670612333, \"16\"],\n[-37.8759115833, 175.4670291, \"14\"],\n[-37.8757069333, 175.4669862167, \"12\"],\n[-37.87555015, 175.4673181667, \"10\"],\n[-37.8754323167, 175.4670474333, \"8\"],\n[-37.8752163333, 175.4671320667, \"6\"],\n[-37.8750437, 175.4671685, \"4\"],\n[-37.87486615, 175.4671727833, \"2\"],\n[-37.8836992833, 175.4725036, \"23A\"],\n[-37.8827955833, 175.47167325, \"34\"],\n[-37.88383525, 175.47247865, \"21B\"],\n[-37.8836711, 175.4721970333, \"23\"],\n[-37.88384845, 175.4721985333, \"21A\"],\n[-37.8832467333, 175.4721379, \"31\"],\n[-37.8852774167, 175.4718275333, \"10\"],\n[-37.8841115833, 175.4722314667, \"17\"],\n[-37.882555, 175.4720817333, \"41\"],\n[-37.8814569667, 175.4715107167, \"46\"],\n[-37.8826952667, 175.4720762667, \"39\"],\n[-37.8834193, 175.4725388, \"27\"],\n[-37.8834190667, 175.47216485, \"29\"],\n[-37.8835037667, 175.4725855, \"25\"],\n[-37.88308205, 175.4721211333, \"33\"],\n[-37.88081245, 175.47212895, \"67A\"],\n[-37.8808169167, 175.4719355833, \"67\"],\n[-37.8854703, 175.4715485833, \"8A\"],\n[-37.8854802667, 175.4723117333, \"7\"],\n[-37.8819775333, 175.4720601833, \"45\"],\n[-37.8820338, 175.4715768333, \"38\"],\n[-37.88505425, 175.4725552, \"11A\"],\n[-37.8816020167, 175.4715219833, \"44\"],\n[-37.8818502167, 175.4715604167, \"40\"],\n[-37.8817451, 175.4715327167, \"42\"],\n[-37.8817693333, 175.4720335, \"47\"],\n[-37.8826490167, 175.4716679, \"36\"],\n[-37.882937, 175.4720998667, \"35\"],\n[-37.8828116667, 175.4720924167, \"37\"],\n[-37.8851022, 175.4718166333, \"12\"],\n[-37.8817387667, 175.47236985, \"49\"],\n[-37.8812409667, 175.4710926333, \"50\"],\n[-37.8815675333, 175.4723074, \"51A\"],\n[-37.8816046833, 175.4720110833, \"51\"],\n[-37.8812419, 175.4709490833, \"52\"],\n[-37.8811635333, 175.4709188667, \"54\"],\n[-37.8814256833, 175.47197785, \"55\"],\n[-37.8811236333, 175.4712705167, \"56\"],\n[-37.8811993, 175.4719696333, \"57\"],\n[-37.88108405, 175.4714977167, \"58\"],\n[-37.8809842667, 175.4719474833, \"59\"],\n[-37.8809340167, 175.4725577, \"61\"],\n[-37.8808571667, 175.47147535, \"62A\"],\n[-37.8808698167, 175.4712961833, \"62B\"],\n[-37.88095885, 175.4727073167, \"63\"],\n[-37.8805953667, 175.471471, \"64\"],\n[-37.8808394, 175.4727179833, \"65\"],\n[-37.8856441667, 175.4723274, \"5\"],\n[-37.8856559833, 175.47193445, \"6A\"],\n[-37.8858045667, 175.4719521, \"6\"],\n[-37.8854608833, 175.4718410833, \"8\"],\n[-37.8852512333, 175.4722922833, \"9\"],\n[-37.8812897833, 175.47150895, \"48\"],\n[-37.8846497167, 175.4717921833, \"18\"],\n[-37.8850509167, 175.4722740667, \"11\"],\n[-37.8849657833, 175.4718045667, \"14\"],\n[-37.8863145, 175.47202075, \"4\"],\n[-37.8863136833, 175.4724193333, \"3\"],\n[-37.8848130833, 175.4725039833, \"13A\"],\n[-37.8848266333, 175.47225735, \"13\"],\n[-37.8839960167, 175.4722167, \"19\"],\n[-37.8848076833, 175.47180565, \"16\"],\n[-37.8847455833, 175.4722526833, \"15\"],\n[-37.8850477, 175.4726911167, \"11B\"],\n[-37.8941738167, 175.4707689, \"84D\"],\n[-37.8940840667, 175.4708939333, \"84C\"],\n[-37.8941785, 175.4709219, \"84B\"],\n[-37.8941324333, 175.47094385, \"84A\"],\n[-37.8935136, 175.4612187667, \"11\"],\n[-37.8939243167, 175.4608987333, \"10\"],\n[-37.8933340667, 175.4717043167, \"100\"],\n[-37.8936952, 175.4650319333, \"42\"],\n[-37.8932683, 175.4655177833, \"41\"],\n[-37.8936863667, 175.46521985, \"44\"],\n[-37.8936738833, 175.4655087667, \"46\"],\n[-37.8938362667, 175.4624257833, \"22\"],\n[-37.8937817, 175.4635414833, \"30\"],\n[-37.8938118, 175.4643993167, \"40A\"],\n[-37.8937340833, 175.4643627, \"40\"],\n[-37.8935297167, 175.4682022667, \"68\"],\n[-37.8932907833, 175.4653249667, \"39\"],\n[-37.8935900167, 175.4670001333, \"54\"],\n[-37.89330695, 175.4650841, \"37\"],\n[-37.8927154833, 175.4709052, \"65B\"],\n[-37.8927901, 175.4708936667, \"65A\"],\n[-37.8927898667, 175.4705552833, \"63A\"],\n[-37.8930308333, 175.461289, \"13A\"],\n[-37.8932869167, 175.46127415, \"13\"],\n[-37.89365705, 175.4657872833, \"48\"],\n[-37.8939401, 175.46390025, \"32A\"],\n[-37.8936144167, 175.4665801333, \"52\"],\n[-37.89383355, 175.4665993833, \"52A\"],\n[-37.8932530667, 175.47349375, \"120\"],\n[-37.8934369833, 175.4623886667, \"19\"],\n[-37.8942868333, 175.4610974, \"12\"],\n[-37.89390075, 175.46130575, \"14\"],\n[-37.8934877, 175.4615844, \"15\"],\n[-37.89438475, 175.4619340667, \"16A\"],\n[-37.89440795, 175.46167145, \"16\"],\n[-37.89348165, 175.4617730167, \"17\"],\n[-37.8938709333, 175.46165795, \"18\"],\n[-37.8933871167, 175.4633617, \"27\"],\n[-37.8934256333, 175.46270255, \"21\"],\n[-37.8934101, 175.4630095833, \"23\"],\n[-37.8938231667, 175.4626998167, \"24\"],\n[-37.8938185, 175.4629043667, \"26\"],\n[-37.8933327, 175.4599851667, \"1\"],\n[-37.8938655, 175.4619277667, \"20\"],\n[-37.8933737667, 175.4636349, \"29\"],\n[-37.8933599167, 175.4638405667, \"31\"],\n[-37.8937605833, 175.4638030667, \"32\"],\n[-37.8933501333, 175.4640728, \"33\"],\n[-37.89409735, 175.4639711333, \"34\"],\n[-37.89333535, 175.46433465, \"35\"],\n[-37.8937382833, 175.4641705, \"36\"],\n[-37.8938094833, 175.46310885, \"28\"],\n[-37.8937422833, 175.4642690333, \"38\"],\n[-37.8932706, 175.46028355, \"3A\"],\n[-37.8934162167, 175.4602382333, \"3\"],\n[-37.8935366333, 175.4605623667, \"5\"],\n[-37.8935312, 175.4607884833, \"7\"],\n[-37.89396845, 175.4602069333, \"8\"],\n[-37.8935128167, 175.4609781167, \"9\"],\n[-37.8932981167, 175.47301005, \"108\"],\n[-37.8932691667, 175.4732267667, \"110\"],\n[-37.8936267667, 175.47335325, \"112A\"],\n[-37.8938256167, 175.4733656, \"112B\"],\n[-37.8936103333, 175.4733669833, \"112\"],\n[-37.8937215333, 175.4735637667, \"114\"],\n[-37.89365715, 175.4738653167, \"116\"],\n[-37.8935390333, 175.4735507333, \"118\"],\n[-37.8934577667, 175.4693120833, \"70\"],\n[-37.8929881833, 175.4703184667, \"59\"],\n[-37.8929812667, 175.4706668833, \"63\"],\n[-37.8929615667, 175.47086305, \"65\"],\n[-37.8929371167, 175.4712434167, \"69\"],\n[-37.8929520667, 175.47105645, \"67\"],\n[-37.89367425, 175.46743525, \"58A\"],\n[-37.8937744667, 175.4673928, \"58B\"],\n[-37.8935667833, 175.46736855, \"58\"],\n[-37.89367375, 175.4675524167, \"60A\"],\n[-37.8937666333, 175.4676028333, \"60B\"],\n[-37.8935516833, 175.46756615, \"60\"],\n[-37.8935477333, 175.4677751333, \"62\"],\n[-37.8935387833, 175.4679283333, \"64\"],\n[-37.8935372333, 175.4680373167, \"66\"],\n[-37.8934518, 175.4694155833, \"72\"],\n[-37.8934506167, 175.4695295167, \"74\"],\n[-37.8934785833, 175.4697087167, \"76\"],\n[-37.8934174, 175.4705130167, \"82\"],\n[-37.8934133, 175.4707143833, \"84\"],\n[-37.89339765, 175.4708925667, \"86\"],\n[-37.8937520167, 175.4710181667, \"88\"],\n[-37.8933705, 175.47113315, \"90\"],\n[-37.8934223167, 175.4702347833, \"78\"],\n[-37.8933635, 175.4712602167, \"92\"],\n[-37.8933623333, 175.4713753167, \"94\"],\n[-37.89334455, 175.4715971333, \"98\"],\n[-37.8933610333, 175.4714828333, \"96\"],\n[-37.89307625, 175.4690541333, \"2/53\"],\n[-37.8930733667, 175.46912325, \"3/53\"],\n[-37.8930706333, 175.4691856, \"4/53\"],\n[-37.89306775, 175.4692547333, \"5/53\"],\n[-37.8930755833, 175.4687906833, \"47\"],\n[-37.8930806333, 175.4689783167, \"1/53\"],\n[-37.8937726, 175.46406345, \"36A\"],\n[-37.8940399167, 175.4626443667, \"24A\"],\n[-37.89306665, 175.4694318, \"57A\"],\n[-37.8929485833, 175.4694292, \"57B\"],\n[-37.893691, 175.4662858833, \"50\"],\n[-37.892929, 175.4695717833, \"57C\"],\n[-37.8818993667, 175.48831235, \"10\"],\n[-37.8819047, 175.4884493667, \"9\"],\n[-37.8818925167, 175.4887367833, \"8\"],\n[-37.8817909833, 175.4886448, \"7\"],\n[-37.88156225, 175.4884151333, \"5\"],\n[-37.8816410667, 175.4884968833, \"6\"],\n[-37.8817882667, 175.4881434333, \"12\"],\n[-37.8820450667, 175.4880255, \"14\"],\n[-37.8820399833, 175.4881601833, \"13\"],\n[-37.8817013833, 175.48815045, \"11\"],\n[-37.8814522333, 175.4882864667, \"4\"],\n[-37.88135345, 175.4881963833, \"3\"],\n[-37.89047955, 175.3660982833, \"48\"],\n[-37.8939863833, 175.3676102167, \"84\"],\n[-37.8908236333, 175.3668215167, \"57\"],\n[-37.8927242667, 175.3669994667, \"72\"],\n[-37.89707325, 175.3696241833, \"111\"],\n[-37.8951834, 175.3681841167, \"96\"],\n[-37.8891679333, 175.3661342, \"25\"],\n[-37.8990757167, 175.4813831667, \"12\"],\n[-37.8990598, 175.4826089667, \"27\"],\n[-37.8996862, 175.48382855, \"34\"],\n[-37.8998746167, 175.4868621833, \"99\"],\n[-37.9001283, 175.4864203833, \"97\"],\n[-37.90028175, 175.4875789833, \"111\"],\n[-37.9004845333, 175.4875032167, \"113\"],\n[-37.9005451667, 175.4877341667, \"115\"],\n[-37.8987095167, 175.48150085, \"15\"],\n[-37.8987892, 175.4817609, \"17\"],\n[-37.8991357, 175.48159105, \"14\"],\n[-37.89891375, 175.4808869667, \"10\"],\n[-37.8985199667, 175.4808965667, \"11\"],\n[-37.8985627833, 175.4811206333, \"13\"],\n[-37.8992842667, 175.4821156167, \"18-20\"],\n[-37.8988612333, 175.48195285, \"19\"],\n[-37.8993973833, 175.4824338, \"22\"],\n[-37.8989453833, 175.48216625, \"23\"],\n[-37.8994684833, 175.4826182833, \"24\"],\n[-37.8989983, 175.48239085, \"25\"],\n[-37.8995365667, 175.4828116833, \"26\"],\n[-37.8982566833, 175.4800233833, \"1\"],\n[-37.8995900333, 175.48303525, \"28\"],\n[-37.8991642833, 175.4829475167, \"29\"],\n[-37.8996510167, 175.4832267667, \"30\"],\n[-37.899284, 175.4832051333, \"31\"],\n[-37.8983305, 175.4802590667, \"3\"],\n[-37.8985982333, 175.4800186167, \"2\"],\n[-37.8995170333, 175.4845078667, \"49\"],\n[-37.8987404667, 175.4802334667, \"4\"],\n[-37.89943635, 175.4849588167, \"77\"],\n[-37.8983839667, 175.4804605, \"5\"],\n[-37.8987906833, 175.4804517, \"6\"],\n[-37.89971175, 175.4849414, \"81\"],\n[-37.8984425333, 175.4806757, \"7\"],\n[-37.8988546833, 175.4806650333, \"8\"],\n[-37.8982073167, 175.4808983833, \"9\"],\n[-37.9002304333, 175.4868039667, \"101\"],\n[-37.8997452667, 175.48737465, \"103\"],\n[-37.9001569667, 175.4873521333, \"105A\"],\n[-37.90035675, 175.4871693, \"105\"],\n[-37.89977645, 175.4853644833, \"85\"],\n[-37.8998911833, 175.4857328667, \"93\"],\n[-37.89830875, 175.4810073667, \"11A\"],\n[-37.8991998, 175.4818039833, \"16\"],\n[-37.9000736, 175.48765565, \"109\"],\n[-37.8998738833, 175.4877091, \"107\"],\n[-37.8584179167, 175.3917306, \"28C\"],\n[-37.8604560167, 175.3943771, \"27\"],\n[-37.8594062, 175.39328765, \"22\"],\n[-37.8602464667, 175.3945281333, \"25\"],\n[-37.8580738, 175.3913907167, \"28B\"],\n[-37.8588583833, 175.3948410167, \"10\"],\n[-37.8597253167, 175.3943257167, \"20\"],\n[-37.8594598667, 175.3949546667, \"17\"],\n[-37.85742675, 175.3934756833, \"28D\"],\n[-37.85779025, 175.3909575667, \"28A\"],\n[-37.8602461333, 175.39392145, \"24\"],\n[-37.8615513167, 175.3930931833, \"30\"],\n[-37.8615702833, 175.3936576333, \"41\"],\n[-37.8648762167, 175.391556, \"85\"],\n[-37.8669371833, 175.38969465, \"116\"],\n[-37.8656294833, 175.39053055, \"96\"],\n[-37.8627939333, 175.3928152833, \"57\"],\n[-37.8642163, 175.3938465167, \"63B\"],\n[-37.8633075167, 175.3925197167, \"63A\"],\n[-37.8752946833, 175.47600165, \"16\"],\n[-37.8752789, 175.4745710667, \"26\"],\n[-37.8752698667, 175.4741175667, \"30\"],\n[-37.8752788833, 175.4747855667, \"24\"],\n[-37.8753729833, 175.4773013667, \"4\"],\n[-37.8752503333, 175.47339515, \"36\"],\n[-37.8753239, 175.4768918333, \"8\"],\n[-37.8752590667, 175.4736461333, \"34\"],\n[-37.8756213167, 175.4747827833, \"25\"],\n[-37.8752682333, 175.4738759333, \"32\"],\n[-37.8752943, 175.47621685, \"14\"],\n[-37.8755946667, 175.4783229167, \"2\"],\n[-37.8757436, 175.47737735, \"7\"],\n[-37.8758519, 175.4778569167, \"3\"],\n[-37.8756174833, 175.4745228, \"27\"],\n[-37.8756574833, 175.4762356, \"15\"],\n[-37.8756050833, 175.4734895, \"33\"],\n[-37.875302, 175.4766701833, \"10\"],\n[-37.8756027333, 175.4736947333, \"31\"],\n[-37.8752780667, 175.4758057, \"18\"],\n[-37.8757905833, 175.477583, \"5\"],\n[-37.8756395833, 175.4758414333, \"19\"],\n[-37.8756402, 175.4760084333, \"17\"],\n[-37.8752715833, 175.4751979, \"20\"],\n[-37.8757130167, 175.4771411333, \"9\"],\n[-37.8752756833, 175.47501635, \"22\"],\n[-37.87527695, 175.47433525, \"28\"],\n[-37.8756425833, 175.4752112667, \"21\"],\n[-37.8752937833, 175.4764377, \"12\"],\n[-37.87559675, 175.4732172, \"35\"],\n[-37.8753504333, 175.477108, \"6\"],\n[-37.8755737333, 175.4729657167, \"37\"],\n[-37.8756244333, 175.4743205667, \"29\"],\n[-37.8755172667, 175.4727099833, \"39\"],\n[-37.8756327333, 175.4750188, \"23\"],\n[-37.8754463333, 175.4724567833, \"41\"],\n[-37.8753804667, 175.4721916167, \"43\"],\n[-37.8753413, 175.4719123167, \"45\"],\n[-37.8751775167, 175.4728065, \"40\"],\n[-37.87523985, 175.4731198167, \"38\"],\n[-37.9129636333, 175.47354695, \"15\"],\n[-37.9129915667, 175.4741582833, \"10\"],\n[-37.9130924, 175.4741147667, \"12\"],\n[-37.91301345, 175.4738697833, \"14\"],\n[-37.9125089, 175.47363755, \"3\"],\n[-37.9126150667, 175.4743185833, \"4\"],\n[-37.9126618333, 175.4735482833, \"5\"],\n[-37.9125892, 175.47392305, \"6\"],\n[-37.9127883167, 175.47388245, \"8\"],\n[-37.9126731333, 175.473192, \"7\"],\n[-37.91275485, 175.47314795, \"9\"],\n[-37.9128106333, 175.4733799667, \"11\"],\n[-37.9123932, 175.4739944667, \"2\"],\n[-37.9123316833, 175.4737106167, \"1\"],\n[-37.9838611667, 175.5750239333, \"209\"],\n[-37.9690023333, 175.5700330333, \"12\"],\n[-37.9711171, 175.5702027833, \"25\"],\n[-37.9779899, 175.570433, \"1/122\"],\n[-37.97820695, 175.57005845, \"2/122\"],\n[-37.9800575, 175.5723918333, \"153\"],\n[-37.9811843, 175.5731835, \"177\"],\n[-37.9754233167, 175.57078555, \"95\"],\n[-37.8838929167, 175.4818853167, \"12\"],\n[-37.8837344667, 175.4818862, \"14\"],\n[-37.8835817667, 175.4818741, \"16\"],\n[-37.8834241, 175.4818550667, \"18\"],\n[-37.8832506167, 175.48183555, \"20\"],\n[-37.8830817667, 175.4818428833, \"22\"],\n[-37.8828965167, 175.48182015, \"24\"],\n[-37.8827122833, 175.4817928, \"26\"],\n[-37.8814133667, 175.4817274333, \"44\"],\n[-37.8813040833, 175.48171155, \"46\"],\n[-37.8810746667, 175.4817141833, \"48\"],\n[-37.8809023, 175.4816841, \"50\"],\n[-37.8807272167, 175.4816891167, \"52\"],\n[-37.8805686833, 175.4816589167, \"54\"],\n[-37.8803979167, 175.4816530833, \"56\"],\n[-37.8801843833, 175.48158535, \"58\"],\n[-37.8825403667, 175.4813836167, \"32A\"],\n[-37.88253505, 175.4817773, \"32\"],\n[-37.8825069167, 175.48115065, \"34\"],\n[-37.88240895, 175.4812873333, \"36\"],\n[-37.8823411, 175.4817443167, \"38\"],\n[-37.8822324, 175.4817659167, \"40\"],\n[-37.8852853, 175.4825293667, \"1\"],\n[-37.8852325667, 175.4820864167, \"2\"],\n[-37.8856616, 175.4830290667, \"3\"],\n[-37.8851546, 175.4819058667, \"4\"],\n[-37.8853310833, 175.4831131333, \"5\"],\n[-37.8851287833, 175.4816127333, \"6\"],\n[-37.8827869667, 175.4815392833, \"26B\"],\n[-37.88484925, 175.4820581167, \"8\"],\n[-37.9024855, 175.4814230667, \"6\"],\n[-37.9027508667, 175.48132635, \"2\"],\n[-37.90267385, 175.4810493833, \"3\"],\n[-37.902509, 175.4810477333, \"4\"],\n[-37.90239855, 175.4812007, \"5\"],\n[-37.8861399167, 175.4659005167, \"2A\"],\n[-37.8858972, 175.4701092167, \"36\"],\n[-37.8867484667, 175.4678494333, \"19\"],\n[-37.8861574833, 175.46575525, \"2\"],\n[-37.8864493167, 175.4662703167, \"3\"],\n[-37.88622685, 175.47015215, \"39\"],\n[-37.88624375, 175.4699778833, \"37\"],\n[-37.8863943167, 175.4703065667, \"39A\"],\n[-37.8858773667, 175.4704395333, \"38\"],\n[-37.8866652833, 175.4663845667, \"3A\"],\n[-37.8862623667, 175.4706270167, \"41\"],\n[-37.8856218833, 175.4704532333, \"40\"],\n[-37.8854065833, 175.4706356333, \"42B\"],\n[-37.88625425, 175.4707564167, \"43\"],\n[-37.88541165, 175.4704709667, \"42A\"],\n[-37.8861368333, 175.47271145, \"55\"],\n[-37.886122, 175.4661553833, \"4\"],\n[-37.8863355167, 175.4685020667, \"27\"],\n[-37.8865969667, 175.4687519167, \"29A\"],\n[-37.8865831, 175.4689422667, \"31A\"],\n[-37.8860333167, 175.46789315, \"20\"],\n[-37.8858932167, 175.4702396667, \"36A\"],\n[-37.88643745, 175.4660770167, \"1\"],\n[-37.8857492, 175.46868685, \"30A\"],\n[-37.8855154167, 175.4686515333, \"30B\"],\n[-37.8856861, 175.46828205, \"24A\"],\n[-37.8860060667, 175.4682543333, \"24\"],\n[-37.8863518833, 175.4681365167, \"23\"],\n[-37.8860244, 175.4680726833, \"22\"],\n[-37.8860988667, 175.4665564833, \"10\"],\n[-37.8863865333, 175.4673575, \"11\"],\n[-37.8863826833, 175.4675404, \"13\"],\n[-37.8863788667, 175.4677124167, \"15\"],\n[-37.8860585833, 175.4674608333, \"14\"],\n[-37.8856044667, 175.46749745, \"16A\"],\n[-37.8857502, 175.46754885, \"16\"],\n[-37.8864570833, 175.4677168333, \"17\"],\n[-37.8860457333, 175.4676692, \"18\"],\n[-37.8867451667, 175.4680089167, \"19A\"],\n[-37.8858168333, 175.4717908, \"58\"],\n[-37.8866057333, 175.4666266833, \"5A\"],\n[-37.8864473333, 175.4665519333, \"5\"],\n[-37.88573305, 175.4662191833, \"6\"],\n[-37.8861234333, 175.4729670667, \"57\"],\n[-37.8861040833, 175.4732588, \"59\"],\n[-37.8857812833, 175.4727290333, \"62\"],\n[-37.8860733167, 175.47367975, \"63\"],\n[-37.8853214167, 175.47302445, \"64A\"],\n[-37.8857647, 175.4729568833, \"64\"],\n[-37.8860630667, 175.4741376333, \"65\"],\n[-37.8855514167, 175.4731432833, \"66A\"],\n[-37.8857605333, 175.4732639667, \"66\"],\n[-37.88604615, 175.4744234333, \"67\"],\n[-37.8857253333, 175.4734757667, \"68\"],\n[-37.8860271667, 175.4749003, \"71\"],\n[-37.8864273833, 175.4668194167, \"7\"],\n[-37.8861134167, 175.4663483833, \"8\"],\n[-37.88530955, 175.4734951667, \"72A\"],\n[-37.8853649167, 175.4735031333, \"72\"],\n[-37.8857145667, 175.47370235, \"74\"],\n[-37.88570935, 175.4738793833, \"76\"],\n[-37.8856962833, 175.4740871833, \"78\"],\n[-37.8855425, 175.4740492667, \"80\"],\n[-37.8855359667, 175.47421665, \"82\"],\n[-37.8856920333, 175.4742632667, \"84\"],\n[-37.88567285, 175.4743871833, \"86\"],\n[-37.8856348, 175.47498665, \"88\"],\n[-37.8864008667, 175.4671270833, \"9\"],\n[-37.8859601833, 175.47522375, \"73\"],\n[-37.8859930167, 175.4754003167, \"75\"],\n[-37.88563045, 175.4752063667, \"90\"],\n[-37.8856196667, 175.4754834167, \"94\"],\n[-37.8855985333, 175.4757720333, \"98\"],\n[-37.8852094333, 175.4756059333, \"96\"],\n[-37.8856225333, 175.4706336667, \"44\"],\n[-37.8863443167, 175.4711711167, \"45A\"],\n[-37.8862241333, 175.47112885, \"45\"],\n[-37.8858591, 175.4707636333, \"46\"],\n[-37.8862151, 175.4713376833, \"47\"],\n[-37.88584785, 175.4710055, \"48\"],\n[-37.8863784667, 175.4716221167, \"49A\"],\n[-37.88619305, 175.4716161667, \"49\"],\n[-37.8854010167, 175.4710779333, \"50A\"],\n[-37.8855194, 175.4710888, \"50\"],\n[-37.8861586167, 175.4718484, \"51\"],\n[-37.8855134333, 175.4712143833, \"52\"],\n[-37.8858417833, 175.4713232833, \"54\"],\n[-37.8858254, 175.4715816333, \"56\"],\n[-37.8859913, 175.4685190667, \"28\"],\n[-37.8863223167, 175.4686848833, \"29\"],\n[-37.8859773333, 175.46879095, \"30\"],\n[-37.8863225167, 175.4688814167, \"31\"],\n[-37.8859628333, 175.4690224667, \"32\"],\n[-37.8863278333, 175.4690195667, \"33\"],\n[-37.8860351333, 175.4746824, \"69\"],\n[-37.88563915, 175.4684005167, \"26\"],\n[-37.8863489, 175.4683084, \"25\"],\n[-37.8866069, 175.4673016, \"11A\"],\n[-37.8868120167, 175.4673208833, \"11B\"],\n[-37.88578115, 175.4680634667, \"22A\"],\n[-37.8863727, 175.4679364, \"21\"],\n[-37.8858363, 175.4659994, \"4A\"],\n[-37.88658795, 175.4674919167, \"13A\"],\n[-37.92234345, 175.4747052, \"199\"],\n[-37.92177305, 175.4769218333, \"2\"],\n[-37.9221347833, 175.47694995, \"1\"],\n[-37.9222941667, 175.47512075, \"165\"],\n[-37.9219244667, 175.4755311667, \"126\"],\n[-37.92194885, 175.4751084333, \"164\"],\n[-37.92227225, 175.4756573333, \"109\"],\n[-37.9226473833, 175.47590455, \"105\"],\n[-37.9217738833, 175.4766539667, \"38\"],\n[-37.9226126333, 175.4765605333, \"45\"],\n[-37.9221432333, 175.4762786, \"51\"],\n[-37.92185925, 175.4759778667, \"80\"],\n[-37.9219880167, 175.4744712833, \"230\"],\n[-37.9213271, 175.4744695667, \"200\"],\n[-37.9210363333, 175.4743909167, \"210\"],\n[-37.9206876333, 175.4744863, \"220\"],\n[-37.9212772667, 175.4748825167, \"184\"],\n[-37.9213329, 175.4754072167, \"194\"],\n[-37.92179965, 175.4763127667, \"50\"],\n[-37.9226488667, 175.4760626667, \"101\"],\n[-37.9224377167, 175.4742912, \"231\"],\n[-37.9222663833, 175.4741209667, \"253\"],\n[-37.9220891, 175.4742094833, \"250\"],\n[-37.8763709833, 175.4737251167, \"6\"],\n[-37.8762928, 175.4740830833, \"7\"],\n[-37.8757872, 175.47372185, \"2\"],\n[-37.8759274833, 175.4737143667, \"4\"],\n[-37.8761889833, 175.4744309833, \"5B\"],\n[-37.8760917667, 175.4744256833, \"5A\"],\n[-37.8760035, 175.47408515, \"3\"],\n[-37.8758006, 175.4741481167, \"1\"],\n[-37.8945807833, 175.4731283, \"92\"],\n[-37.8952838, 175.4659743667, \"9\"],\n[-37.8945913667, 175.47286075, \"88\"],\n[-37.8948307167, 175.4678840833, \"26\"],\n[-37.8946495333, 175.4715883833, \"68\"],\n[-37.8955784167, 175.4666195833, \"17\"],\n[-37.8950308833, 175.4707890333, \"55\"],\n[-37.8950268333, 175.4708643, \"57\"],\n[-37.8950210333, 175.4709645333, \"59\"],\n[-37.8950726167, 175.4692980667, \"35\"],\n[-37.8948167167, 175.4681206667, \"28\"],\n[-37.89480715, 175.4683576167, \"30\"],\n[-37.89448235, 175.47392275, \"100\"],\n[-37.8949025833, 175.4736886667, \"101\"],\n[-37.8944604833, 175.4740989333, \"102\"],\n[-37.8940345667, 175.4741795333, \"104\"],\n[-37.8948037, 175.4748156667, \"105\"],\n[-37.8944383833, 175.4744140333, \"106\"],\n[-37.8949698833, 175.4659329667, \"10\"],\n[-37.8949612167, 175.4663892333, \"12\"],\n[-37.8949153, 175.46660965, \"14\"],\n[-37.8948991667, 175.46678365, \"16\"],\n[-37.8947661, 175.46694865, \"18A\"],\n[-37.8944432333, 175.4669301, \"18B\"],\n[-37.8948852, 175.4669605667, \"18\"],\n[-37.8948780167, 175.46712785, \"20\"],\n[-37.8944023, 175.4672810833, \"22B\"],\n[-37.8947610167, 175.46729965, \"22A\"],\n[-37.8948679667, 175.46733215, \"22\"],\n[-37.8948783833, 175.46748345, \"1/24-7/24\"],\n[-37.8947230167, 175.4702766667, \"42\"],\n[-37.8946909667, 175.4707959333, \"54\"],\n[-37.8946726833, 175.4711023333, \"60\"],\n[-37.8946703833, 175.4712066833, \"62\"],\n[-37.8946656, 175.4712626667, \"64\"],\n[-37.8949888833, 175.4656431, \"6\"],\n[-37.89493175, 175.4729810833, \"93\"],\n[-37.8945739333, 175.47341895, \"94\"],\n[-37.8949277, 175.4731673, \"95\"],\n[-37.89438165, 175.4736724333, \"96A\"],\n[-37.8945503667, 175.4736765833, \"96\"],\n[-37.8949105333, 175.47340155, \"97A\"],\n[-37.8942335833, 175.4737639667, \"98\"],\n[-37.8946295667, 175.4718361, \"72\"],\n[-37.89462675, 175.4719766167, \"74\"],\n[-37.8949939167, 175.4719069333, \"77\"],\n[-37.8946185167, 175.4722803667, \"82\"],\n[-37.8949549, 175.4725255167, \"87\"],\n[-37.8949826333, 175.4657729, \"8\"],\n[-37.8952739667, 175.4662657333, \"11\"],\n[-37.8954714167, 175.4664023833, \"13A\"],\n[-37.8955781667, 175.46640275, \"13B\"],\n[-37.8952808167, 175.4663722, \"13\"],\n[-37.89527365, 175.4665192833, \"15\"],\n[-37.8956551167, 175.4661953667, \"11B\"],\n[-37.8953330833, 175.4650771, \"1\"],\n[-37.8950785667, 175.4691175167, \"33\"],\n[-37.8950110333, 175.4654238667, \"2\"],\n[-37.8950599167, 175.4704818, \"45\"],\n[-37.8950526167, 175.4705562667, \"47\"],\n[-37.8950517333, 175.4706304167, \"49\"],\n[-37.8950748667, 175.4694276667, \"41\"],\n[-37.8953314667, 175.4653118333, \"3\"],\n[-37.8950057833, 175.46551395, \"4\"],\n[-37.8950353, 175.4734756833, \"97\"],\n[-37.8950013, 175.4717792, \"75\"],\n[-37.8950426833, 175.4706778167, \"51\"],\n[-37.89503905, 175.4707278833, \"53\"],\n[-37.8950147333, 175.4710415167, \"61\"],\n[-37.8950166833, 175.4711306, \"63\"],\n[-37.8950141, 175.4712134833, \"65\"],\n[-37.8950130833, 175.4712694167, \"67\"],\n[-37.8950105, 175.4713271, \"69\"],\n[-37.8950101667, 175.4713877667, \"71\"],\n[-37.8950061333, 175.4714993167, \"73\"],\n[-37.8953044, 175.4655703333, \"5\"],\n[-37.8953043, 175.4657896167, \"7\"],\n[-37.8948314667, 175.46896985, \"36\"],\n[-37.8944233667, 175.4670945167, \"20B\"],\n[-37.89476705, 175.4671119, \"20A\"],\n[-37.8948155167, 175.46873695, \"34\"],\n[-37.8785435, 175.4391193, \"108\"],\n[-37.85482825, 175.43943885, \"372\"],\n[-37.8843674667, 175.44077705, \"44\"],\n[-37.8514708333, 175.44301855, \"414\"],\n[-37.8786210833, 175.4386722333, \"107\"],\n[-37.8536346167, 175.43854, \"2/377\"],\n[-37.88482555, 175.43861845, \"37\"],\n[-37.85393405, 175.4384665833, \"1/377\"],\n[-37.87792065, 175.43870995, \"115\"],\n[-37.8616766, 175.43942835, \"296\"],\n[-37.8758269167, 175.439114, \"136\"],\n[-37.8611102167, 175.4393358, \"302\"],\n[-37.8630311167, 175.4365396167, \"279\"],\n[-37.8653560833, 175.4373495833, \"253\"],\n[-37.8757949833, 175.4386379333, \"137\"],\n[-37.8786179833, 175.4342996333, \"2/105\"],\n[-37.8874004, 175.4391212667, \"1/10-2/10\"],\n[-37.8447363167, 175.43930545, \"482\"],\n[-37.88403545, 175.4386151333, \"45\"],\n[-37.8717442667, 175.4349390667, \"183\"],\n[-37.8767875333, 175.4386578667, \"127\"],\n[-37.8797060833, 175.4386293833, \"93\"],\n[-37.8866638167, 175.4390628, \"18\"],\n[-37.8791973, 175.4386315, \"97\"],\n[-37.88585575, 175.4362985167, \"19\"],\n[-37.8790987667, 175.4391399833, \"100\"],\n[-37.8771662, 175.43926595, \"124\"],\n[-37.8838361, 175.4391195833, \"48\"],\n[-37.8857025667, 175.4423457333, \"30\"],\n[-37.8854774667, 175.4423092, \"3/30\"],\n[-37.8449495333, 175.43932215, \"480\"],\n[-37.8540070333, 175.4394581833, \"376\"],\n[-37.8831838833, 175.4385145, \"55\"],\n[-37.8588749833, 175.4393561333, \"326\"],\n[-37.86279375, 175.4385341667, \"281\"],\n[-37.8445601333, 175.43930445, \"484\"],\n[-37.84437095, 175.43930065, \"486\"],\n[-37.8788133833, 175.4351573667, \"1/105\"],\n[-37.8800643333, 175.4356186, \"1/93\"],\n[-37.87918835, 175.4355875333, \"2/93\"],\n[-37.88086735, 175.4358693833, \"83\"],\n[-37.88697085, 175.4385406333, \"15\"],\n[-37.8505857167, 175.4386953333, \"425\"],\n[-37.8857656333, 175.4386266333, \"21\"],\n[-37.8517299333, 175.43952155, \"404\"],\n[-37.8859354667, 175.4391316833, \"20\"],\n[-37.8643110167, 175.43852915, \"269\"],\n[-37.8638944167, 175.4352222, \"271\"],\n[-37.8629044167, 175.4394276667, \"280\"],\n[-37.8636698333, 175.4384519167, \"273\"],\n[-37.87065885, 175.4387204333, \"197\"],\n[-37.870552, 175.4392104333, \"198\"],\n[-37.86847865, 175.4362560333, \"215\"],\n[-37.8682894833, 175.4387331667, \"221\"],\n[-37.8723437333, 175.4386659167, \"179\"],\n[-37.8713473833, 175.4386830333, \"191\"],\n[-37.8803684, 175.4391223, \"84\"],\n[-37.8801818167, 175.4386532333, \"89\"],\n[-37.8795138833, 175.4391232667, \"98\"],\n[-37.88233015, 175.4385688167, \"67\"],\n[-37.8822730833, 175.439127, \"68\"],\n[-37.8815140333, 175.4385229167, \"75\"],\n[-37.88161725, 175.43921275, \"76\"],\n[-37.88298115, 175.4391210167, \"60\"],\n[-37.88356985, 175.44183935, \"54\"],\n[-37.88688885, 175.4390723333, \"16\"],\n[-37.8854630667, 175.44062525, \"1/30\"],\n[-37.8857315833, 175.4406345667, \"2/30\"],\n[-37.8850814333, 175.4391024667, \"32\"],\n[-37.85615335, 175.4394051333, \"358\"],\n[-37.8484322833, 175.4386304333, \"451\"],\n[-37.849595, 175.4395278833, \"438\"],\n[-37.8685579333, 175.43929385, \"222\"],\n[-37.8673376667, 175.4387301333, \"231\"],\n[-37.8672263, 175.4391822833, \"234\"],\n[-37.8658104, 175.4384541833, \"249\"],\n[-37.8650818333, 175.4384285833, \"257\"],\n[-37.8572850833, 175.4388435667, \"345\"],\n[-37.8695889833, 175.4385987333, \"207\"],\n[-37.8444514833, 175.4388799167, \"483\"],\n[-37.8828889333, 175.4356993, \"51\"],\n[-37.8832340833, 175.4358504, \"1/51\"],\n[-37.8573635, 175.4392901833, \"342\"],\n[-37.86163445, 175.43881845, \"295\"],\n[-37.8624819833, 175.438809, \"287\"],\n[-37.84347995, 175.4385449833, \"495\"],\n[-37.85704685, 175.4422632333, \"346\"],\n[-37.8881785167, 175.4536787833, \"11\"],\n[-37.8876931833, 175.4532197833, \"5\"],\n[-37.8877175833, 175.4536870667, \"7\"],\n[-37.8883681, 175.4535562333, \"2/10\"],\n[-37.88862675, 175.4537201667, \"4/10\"],\n[-37.88857235, 175.4536184167, \"3/10\"],\n[-37.88834935, 175.45339255, \"1/10\"],\n[-37.8880737833, 175.4534382833, \"9\"],\n[-37.88862905, 175.4582377167, \"8\"],\n[-37.888388, 175.4577553167, \"5A\"],\n[-37.8890413167, 175.4584074, \"10A\"],\n[-37.8890455167, 175.45818135, \"10\"],\n[-37.8893714, 175.4579083667, \"1\"],\n[-37.8891550333, 175.4579009333, \"2\"],\n[-37.8889156, 175.45789245, \"3\"],\n[-37.8887209667, 175.4578687833, \"4\"],\n[-37.8885121, 175.45785425, \"5\"],\n[-37.8884355167, 175.4579579333, \"6\"],\n[-37.8884670667, 175.4581608667, \"7\"],\n[-37.88885115, 175.4581993167, \"9\"],\n[-37.88829695, 175.4581456, \"7A\"],\n[-37.89493095, 175.50293945, \"2/207\"],\n[-37.8939846833, 175.4919359167, \"1/143\"],\n[-37.9326599333, 175.5720857333, \"946\"],\n[-37.8940428667, 175.4929087, \"2/143\"],\n[-37.93985015, 175.5776648, \"1006\"],\n[-37.9234984333, 175.5621109333, \"816\"],\n[-37.9374057, 175.57533755, \"1004\"],\n[-37.92045495, 175.5594362667, \"773\"],\n[-37.9370037333, 175.5752245333, \"1002\"],\n[-37.9233134, 175.5619061667, \"814\"],\n[-37.9247701333, 175.5642938333, \"838\"],\n[-37.9091874667, 175.5318147333, \"504\"],\n[-37.90968075, 175.53436585, \"525\"],\n[-37.9100798667, 175.53351915, \"518\"],\n[-37.9137304333, 175.5349179, \"560\"],\n[-37.9131590333, 175.53488155, \"558\"],\n[-37.9072097667, 175.5257719, \"458\"],\n[-37.89594205, 175.4996267667, \"178\"],\n[-37.9372702, 175.57594035, \"1013\"],\n[-37.89638255, 175.5065894833, \"246\"],\n[-37.8968645833, 175.50876055, \"259\"],\n[-37.8980340833, 175.5096747833, \"276\"],\n[-37.8986749667, 175.5105449167, \"280\"],\n[-37.89924075, 175.5129982667, \"295\"],\n[-37.9015737, 175.515433, \"324\"],\n[-37.91571275, 175.5534373, \"705\"],\n[-37.9162442833, 175.5534827, \"706\"],\n[-37.9148016833, 175.54767885, \"648\"],\n[-37.9148501, 175.5499118333, \"673\"],\n[-37.9151485667, 175.5495203167, \"660\"],\n[-37.9197264, 175.5575957333, \"748\"],\n[-37.9212959167, 175.5604435167, \"787\"],\n[-37.9255722167, 175.5660694167, \"852\"],\n[-37.9026268333, 175.5172354667, \"356\"],\n[-37.8950368333, 175.5025268833, \"1/207\"],\n[-37.9122779833, 175.5393717167, \"2/573\"],\n[-37.9114679667, 175.5398593333, \"3/573\"],\n[-37.91540075, 175.5527015167, \"699\"],\n[-37.9012012167, 175.51619825, \"335\"],\n[-37.9168711667, 175.5553676, \"719\"],\n[-37.9175395667, 175.5561893167, \"723\"],\n[-37.9180392833, 175.5566833, \"739\"],\n[-37.9130946167, 175.5474586167, \"637\"],\n[-37.8940036333, 175.4831404833, \"29\"],\n[-37.8943207667, 175.4865820833, \"69\"],\n[-37.8943952833, 175.4886118, \"71\"],\n[-37.8951511833, 175.4905976667, \"112\"],\n[-37.90356905, 175.5202661333, \"379\"],\n[-37.9042539, 175.5213877833, \"391\"],\n[-37.9058724667, 175.5242790667, \"427\"],\n[-37.9355250333, 175.57499645, \"987\"],\n[-37.9134563333, 175.5426692667, \"603\"],\n[-37.9135913667, 175.54315755, \"607\"],\n[-37.91401355, 175.5428852833, \"608\"],\n[-37.9106131167, 175.5359661167, \"539\"],\n[-37.9111353167, 175.53571595, \"542\"],\n[-37.9113920167, 175.5360293167, \"546\"],\n[-37.9121793167, 175.5377621, \"568\"],\n[-37.9346634, 175.5743922333, \"979\"],\n[-37.9374423833, 175.5674758833, \"972\"],\n[-37.9156359, 175.55118655, \"692\"],\n[-37.9155415167, 175.5508029, \"690\"],\n[-37.90281625, 175.51905675, \"373\"],\n[-37.9027791333, 175.5175252833, \"358\"],\n[-37.8958690833, 175.5018481167, \"190\"],\n[-37.9789857333, 175.4405075667, \"168\"],\n[-37.9695422, 175.4324544167, \"23\"],\n[-37.9694634, 175.4313197333, \"12\"],\n[-37.9734056167, 175.4391843667, \"105\"],\n[-37.9748037833, 175.4419470833, \"123\"],\n[-37.9775236, 175.4415263333, \"155\"],\n[-37.9782757667, 175.44079755, \"166\"],\n[-37.9718126167, 175.4347789833, \"54\"],\n[-37.8974829, 175.4523839667, \"11\"],\n[-37.896949, 175.4538974333, \"10\"],\n[-37.8973599167, 175.4533428167, \"3\"],\n[-37.89717915, 175.4544804167, \"4\"],\n[-37.8973309833, 175.45308765, \"5\"],\n[-37.8972213833, 175.4542655167, \"6\"],\n[-37.8972975167, 175.452858, \"7\"],\n[-37.8971736167, 175.4540106333, \"8\"],\n[-37.8972301333, 175.4525011667, \"9\"],\n[-37.8962831, 175.4514132833, \"40\"],\n[-37.8962904167, 175.4509267333, \"42\"],\n[-37.8964889167, 175.4515006167, \"44\"],\n[-37.8966648, 175.4514576667, \"46\"],\n[-37.8963241333, 175.4516917333, \"38\"],\n[-37.8971486, 175.4537583167, \"12\"],\n[-37.8974914833, 175.4522686333, \"13\"],\n[-37.8968280167, 175.4517962167, \"32\"],\n[-37.8965602833, 175.4518541, \"34\"],\n[-37.8963553, 175.4518366333, \"36\"],\n[-37.8972173667, 175.4551419333, \"2\"],\n[-37.8970665667, 175.45323925, \"14\"],\n[-37.8971966, 175.4522688167, \"15\"],\n[-37.8968064, 175.4532278167, \"16\"],\n[-37.8967456833, 175.4531232333, \"18\"],\n[-37.8974595167, 175.4539856833, \"1\"],\n[-37.89702105, 175.4529966167, \"20\"],\n[-37.89697325, 175.4525840667, \"22\"],\n[-37.8966754833, 175.45262025, \"24\"],\n[-37.8965024, 175.4525581333, \"26\"],\n[-37.8967277167, 175.4524711167, \"28\"],\n[-37.8969411167, 175.4523854, \"30\"],\n[-37.8971252667, 175.4517190167, \"17\"],\n[-37.8973954333, 175.4516434667, \"19\"],\n[-37.89734885, 175.4512056667, \"27\"],\n[-37.8975603333, 175.4513639167, \"23\"],\n[-37.8975662667, 175.45155165, \"21\"],\n[-37.89758735, 175.45118755, \"25\"],\n[-37.8976514, 175.4505551833, \"25A\"],\n[-37.93618265, 175.4679452833, \"99\"],\n[-37.9364427, 175.47434405, \"47\"],\n[-37.93602995, 175.4635648833, \"143\"],\n[-37.9363754167, 175.4732709667, \"55\"],\n[-37.9364611, 175.47575415, \"37\"],\n[-37.9362570167, 175.4694700833, \"89\"],\n[-37.9356639667, 175.4542221333, \"225\"],\n[-37.9365317333, 175.4564987833, \"201\"],\n[-37.9357644333, 175.45652615, \"1/201\"],\n[-37.9356391667, 175.4555529667, \"209\"],\n[-37.9359229, 175.4614506167, \"157\"],\n[-37.9354519, 175.4510958, \"245\"],\n[-37.8043647667, 175.3646767, \"9\"],\n[-37.8033645333, 175.3644417333, \"14\"],\n[-37.8041373, 175.3641572833, \"11\"],\n[-37.9559902833, 175.4358548667, \"1/3116\"],\n[-37.9541933, 175.43567675, \"3126\"],\n[-37.9159823833, 175.4458315667, \"2/3627\"],\n[-37.91406535, 175.4497318167, \"3668\"],\n[-37.9555601667, 175.4360378833, \"2/3116\"],\n[-37.9139425833, 175.4497730667, \"3666\"],\n[-37.9160902, 175.4453786667, \"1/3627\"],\n[-37.9619189333, 175.4331309167, \"3032\"],\n[-37.9609963833, 175.4328218833, \"3039\"],\n[-37.9616219333, 175.4333006833, \"3036\"],\n[-37.9329983333, 175.4291615333, \"3361\"],\n[-37.9152040667, 175.4487561167, \"3654\"],\n[-37.9319928333, 175.4302175167, \"3384\"],\n[-37.91578065, 175.4478569, \"3642\"],\n[-37.9157454833, 175.4465326333, \"3635\"],\n[-37.9151056, 175.4479749833, \"3643\"],\n[-37.91545865, 175.4473662167, \"3639\"],\n[-37.9155843167, 175.4470310167, \"3637\"],\n[-37.9027715667, 175.45210165, \"3794\"],\n[-37.9032760833, 175.4549883, \"3784\"],\n[-37.9748930833, 175.4242103667, \"2855\"],\n[-37.9773033833, 175.42065365, \"2829\"],\n[-37.9444458667, 175.4188207667, \"3217\"],\n[-37.9012992833, 175.4500824333, \"3807\"],\n[-37.9127339333, 175.4503371, \"3682\"],\n[-37.90071495, 175.4536467833, \"3829\"],\n[-37.9048702167, 175.45171675, \"2/3774\"],\n[-37.9001265, 175.4627736833, \"3910\"],\n[-37.8984810667, 175.4580702, \"2/3879\"],\n[-37.8987156833, 175.45782225, \"3/3879\"],\n[-37.8996284833, 175.4598028333, \"3886\"],\n[-37.91662675, 175.4488524167, \"3648\"],\n[-37.9105417167, 175.4505777, \"3714\"],\n[-37.9047285333, 175.4517519, \"1/3774\"],\n[-37.98387015, 175.4128784667, \"2/2724\"],\n[-37.9836752333, 175.4125610667, \"1/2724\"],\n[-37.8997874, 175.4596524, \"2/3886\"],\n[-37.8999206167, 175.4595150667, \"3/3886\"],\n[-37.9052384833, 175.4516238333, \"3/3774\"],\n[-37.9009602667, 175.4546291667, \"3846\"],\n[-37.89997485, 175.4588545167, \"3890\"],\n[-37.9160101833, 175.4469321333, \"3636\"],\n[-37.9504464333, 175.43548145, \"3164\"],\n[-37.90139465, 175.4518174, \"3808\"],\n[-37.9003306333, 175.4547485333, \"3831\"],\n[-37.9150642, 175.4457459, \"3631\"],\n[-37.9700292833, 175.4302066, \"2954\"],\n[-37.9719347833, 175.4284579333, \"2920\"],\n[-37.9684611833, 175.4302354667, \"2959\"],\n[-37.9638249667, 175.4310026333, \"3011\"],\n[-37.9635499667, 175.4311723667, \"3013\"],\n[-37.9621262833, 175.4321201167, \"3025\"],\n[-37.9599553667, 175.4336584667, \"3059\"],\n[-37.9593913167, 175.4346581667, \"3072\"],\n[-37.9671775, 175.4315904833, \"2978\"],\n[-37.9654828167, 175.4323972833, \"2996\"],\n[-37.9304655333, 175.4298457167, \"3403\"],\n[-37.9397400667, 175.4320840667, \"3292\"],\n[-37.9397871333, 175.4311546, \"3299\"],\n[-37.93902, 175.4299714, \"3301\"],\n[-37.93884655, 175.4297564667, \"3311\"],\n[-37.9378664, 175.42977575, \"3320\"],\n[-37.9375432, 175.42891805, \"3321\"],\n[-37.9370408, 175.4289005833, \"3331\"],\n[-37.9364621167, 175.4289989833, \"3337\"],\n[-37.9358432667, 175.4290959667, \"3339\"],\n[-37.95477045, 175.4310178833, \"3103\"],\n[-37.9508932667, 175.4345944, \"3163\"],\n[-37.9502843833, 175.4345016333, \"3169\"],\n[-37.9476339333, 175.4348761333, \"1/3200\"],\n[-37.9459382167, 175.4300649167, \"3215\"],\n[-37.9294075333, 175.4304862, \"3414\"],\n[-37.9292768667, 175.4300710167, \"3415\"],\n[-37.9481910333, 175.43494365, \"2/3200\"],\n[-37.9276201333, 175.4302237167, \"3433\"],\n[-37.9263022167, 175.4323864333, \"3456\"],\n[-37.9577552167, 175.43514095, \"3090\"],\n[-37.9559199, 175.4356498167, \"3110\"],\n[-37.9556312, 175.4352157667, \"3111\"],\n[-37.9541667833, 175.4350528333, \"3127\"],\n[-37.924353, 175.43477375, \"3488\"],\n[-37.92140285, 175.4361990333, \"3521\"],\n[-37.9216205, 175.4367220333, \"3522\"],\n[-37.9199150167, 175.4372039667, \"3537\"],\n[-37.9191848333, 175.4377457167, \"3553\"],\n[-37.91800785, 175.4398907667, \"3571\"],\n[-37.9175554, 175.4427753167, \"3596\"],\n[-37.9167918667, 175.4434860333, \"3611\"],\n[-37.9154718333, 175.4484893833, \"3650\"],\n[-37.91254, 175.4489284667, \"3693\"],\n[-37.9096463167, 175.4500162167, \"3715\"],\n[-37.9095109, 175.45076725, \"3718\"],\n[-37.90312135, 175.4513793, \"3791\"],\n[-37.9025248833, 175.4514848833, \"3797\"],\n[-37.9027026, 175.4521118333, \"3796\"],\n[-37.9012756, 175.4523393333, \"3809\"],\n[-37.9000247833, 175.4550734667, \"3847\"],\n[-37.8996109167, 175.45623955, \"3853\"],\n[-37.8990466, 175.4570460167, \"3861\"],\n[-37.8982341833, 175.4584695333, \"1/3879\"],\n[-37.9037354333, 175.4513122, \"3783\"],\n[-37.9164707667, 175.4457016833, \"3626\"],\n[-37.9005404, 175.4554475167, \"3838\"],\n[-37.92350345, 175.4354481833, \"3498\"],\n[-37.92093435, 175.4372154, \"3528\"],\n[-37.9733884167, 175.4270654167, \"2900\"],\n[-37.9735011833, 175.426221, \"2883\"],\n[-37.9737880333, 175.4258421667, \"2881\"],\n[-37.9736479167, 175.4268099667, \"2882\"],\n[-37.8857180167, 175.4319128667, \"1/1699\"],\n[-37.8762356, 175.4124507167, \"1498\"],\n[-37.8760383833, 175.4119469333, \"1494\"],\n[-37.8881260667, 175.4328696, \"2/1715\"],\n[-37.8866376, 175.4333463667, \"1/1715\"],\n[-37.8884282, 175.4323350167, \"1714\"],\n[-37.8881726667, 175.44522875, \"1835A\"],\n[-37.8828618833, 175.42668015, \"1643\"],\n[-37.8875062667, 175.4321305667, \"1713\"],\n[-37.8891637, 175.4487508333, \"1863\"],\n[-37.8890458667, 175.4450934833, \"1835\"],\n[-37.8893503667, 175.4505826167, \"1881\"],\n[-37.8892833167, 175.4493800333, \"1871\"],\n[-37.88967995, 175.44903065, \"1866\"],\n[-37.8897650833, 175.45173775, \"1894\"],\n[-37.88972655, 175.4509517833, \"1886\"],\n[-37.8893972, 175.4519753167, \"1895\"],\n[-37.8893728833, 175.4515209833, \"1891\"],\n[-37.8893832667, 175.4517583333, \"1893\"],\n[-37.88975065, 175.4512729833, \"1888\"],\n[-37.8897555167, 175.4524542333, \"1896\"],\n[-37.8789651833, 175.4203288667, \"1567\"],\n[-37.8891086, 175.4330816833, \"1716\"],\n[-37.8749076, 175.4095179667, \"1466\"],\n[-37.8766546833, 175.41371075, \"1510\"],\n[-37.87762145, 175.4159128833, \"1532\"],\n[-37.8778396333, 175.41638935, \"1534\"],\n[-37.8784475833, 175.4175476, \"1544\"],\n[-37.8789228667, 175.4189449833, \"1560\"],\n[-37.8838774833, 175.4277925667, \"1659\"],\n[-37.8865829, 175.4310726667, \"2/1699\"],\n[-37.8869873, 175.4315526833, \"1705\"],\n[-37.8887145167, 175.4342103167, \"1735\"],\n[-37.8886082667, 175.4386434, \"1786\"],\n[-37.8888977833, 175.4397067833, \"1788\"],\n[-37.8889447833, 175.4399386, \"1790\"],\n[-37.8891184, 175.4403393, \"1800\"],\n[-37.8886052167, 175.44114285, \"1811\"],\n[-37.8890688833, 175.4442045667, \"1829\"],\n[-37.8894286667, 175.4442416167, \"1830\"],\n[-37.8894495167, 175.4447870333, \"1832\"],\n[-37.8894350833, 175.4526888167, \"1903\"],\n[-37.8898216167, 175.45284305, \"1902\"],\n[-37.8898244333, 175.45305355, \"1906\"],\n[-37.8895077667, 175.4534430333, \"1907\"],\n[-37.8895153, 175.4539427833, \"1913\"],\n[-37.8820731, 175.4246370667, \"1/1628\"],\n[-37.8829790333, 175.42515415, \"2/1628\"],\n[-37.8829182333, 175.42331235, \"3/1628\"],\n[-37.8846927167, 175.4287510833, \"1669\"],\n[-37.8857990667, 175.4293535333, \"1682\"],\n[-37.8859693167, 175.4303029167, \"1/1689\"],\n[-37.8861213333, 175.4304782, \"1689\"],\n[-37.9054227833, 175.4822613, \"5\"],\n[-37.90567515, 175.4819121167, \"6\"],\n[-37.9058446667, 175.4825176167, \"10\"],\n[-37.9057151167, 175.4832514, \"11\"],\n[-37.9059177, 175.4827449333, \"12\"],\n[-37.9062382833, 175.48278225, \"14\"],\n[-37.9059932667, 175.4830166333, \"16\"],\n[-37.90537935, 175.4821119, \"3\"],\n[-37.9055974, 175.4816880167, \"4\"],\n[-37.9055800167, 175.4827762667, \"7\"],\n[-37.9053443333, 175.4819134167, \"1\"],\n[-37.9056483167, 175.4830068333, \"9\"],\n[-37.9687541667, 175.5740302667, \"1149\"],\n[-37.9203192833, 175.5148173833, \"4/319\"],\n[-37.9566814833, 175.5624372, \"1/978\"],\n[-37.95688745, 175.5621776167, \"2/978\"],\n[-37.9580920167, 175.5647472333, \"995\"],\n[-37.92775475, 175.5378236667, \"2/568\"],\n[-37.92609085, 175.5347061333, \"1/542\"],\n[-37.9266186167, 175.5349056333, \"2/542\"],\n[-37.9269626167, 175.5353566167, \"3/542\"],\n[-37.9208396833, 175.5299344667, \"422\"],\n[-37.9207653333, 175.5242669667, \"387\"],\n[-37.9178039333, 175.49454775, \"140\"],\n[-37.9456599, 175.5548334167, \"2/836\"],\n[-37.9455115167, 175.5542481167, \"1/836\"],\n[-37.9194724833, 175.5061424833, \"238\"],\n[-37.9193864333, 175.5055969833, \"232\"],\n[-37.9175102167, 175.49730755, \"159\"],\n[-37.9307227667, 175.53812715, \"1/618-10/618\"],\n[-37.9369806, 175.5416204833, \"648A\"],\n[-37.93400485, 175.5405088167, \"648\"],\n[-37.9352812167, 175.5418752167, \"656\"],\n[-37.9261541333, 175.5367970667, \"564\"],\n[-37.9276279, 175.5381303167, \"1/568\"],\n[-37.9461318333, 175.5545134167, \"842\"],\n[-37.94315595, 175.5537148, \"806\"],\n[-37.9464287167, 175.5549129667, \"844\"],\n[-37.9713060833, 175.57780675, \"1198\"],\n[-37.9403528, 175.5492173333, \"748\"],\n[-37.9406445667, 175.5496667833, \"1/750\"],\n[-37.9282707, 175.5363209167, \"7/568\"],\n[-37.9188207333, 175.505113, \"225\"],\n[-37.9388163333, 175.5467693, \"714\"],\n[-37.9405476, 175.5503991667, \"749\"],\n[-37.94102555, 175.55021985, \"750\"],\n[-37.94425745, 175.5552436333, \"819\"],\n[-37.9459448333, 175.55501645, \"838\"],\n[-37.9465835833, 175.5559441, \"845\"],\n[-37.9472323167, 175.5550552833, \"850\"],\n[-37.9479511833, 175.5562512667, \"1/861\"],\n[-37.9481898667, 175.55635715, \"2/861\"],\n[-37.9500993333, 175.5559819167, \"888\"],\n[-37.95089095, 175.5580154, \"899\"],\n[-37.9289862833, 175.5364012833, \"6/568\"],\n[-37.9653755, 175.5702818667, \"1099\"],\n[-37.9299398667, 175.5377803667, \"3/610\"],\n[-37.9297131333, 175.5382649, \"2/610\"],\n[-37.92950415, 175.5389677333, \"1/610\"],\n[-37.9285464333, 175.53936505, \"601\"],\n[-37.92981565, 175.5365770833, \"568\"],\n[-37.9335473333, 175.5403341667, \"646\"],\n[-37.9331034167, 175.5400956, \"642\"],\n[-37.9327759167, 175.538732, \"640\"],\n[-37.9334526333, 175.5387036667, \"638\"],\n[-37.93320675, 175.5383744833, \"636\"],\n[-37.9336129167, 175.5375626, \"634\"],\n[-37.93288865, 175.5381226667, \"632\"],\n[-37.9327198833, 175.53720065, \"630\"],\n[-37.93255565, 175.53790585, \"628\"],\n[-37.93178035, 175.5394509333, \"626\"],\n[-37.9186410667, 175.5002110833, \"182\"],\n[-37.9172343667, 175.4951996667, \"139\"],\n[-37.9180053667, 175.5002946167, \"183\"],\n[-37.9174022167, 175.4964858167, \"151\"],\n[-37.9234301333, 175.5355513833, \"516\"],\n[-37.9434775667, 175.5532109833, \"808\"],\n[-37.93984305, 175.5484721667, \"742\"],\n[-37.9705266833, 175.57732895, \"1175\"],\n[-37.9272626167, 175.53717615, \"3/568\"],\n[-37.9532905667, 175.5580751833, \"926\"],\n[-37.9528798333, 175.5576940667, \"924\"],\n[-37.9659760333, 175.5710443833, \"1/1111\"],\n[-37.9396966667, 175.5481831, \"740\"],\n[-37.9128371, 175.4832793167, \"19\"],\n[-37.9140123833, 175.4838446333, \"26\"],\n[-37.9172423167, 175.4912675833, \"106\"],\n[-37.91736885, 175.4918037833, \"110\"],\n[-37.91754985, 175.4930980167, \"120\"],\n[-37.9169178833, 175.4926386, \"121\"],\n[-37.9163286667, 175.4889877833, \"85\"],\n[-37.9169649167, 175.4891728833, \"86\"],\n[-37.9165898167, 175.4905852833, \"97\"],\n[-37.9139972667, 175.4850735, \"37\"],\n[-37.9158482667, 175.48707945, \"68\"],\n[-37.9195504667, 175.5065384833, \"240\"],\n[-37.9197355333, 175.5077362, \"256\"],\n[-37.9187252667, 175.5091617167, \"263\"],\n[-37.9196476833, 175.5108055333, \"277\"],\n[-37.9202849333, 175.5111623667, \"300\"],\n[-37.9206995667, 175.5135292833, \"1/314\"],\n[-37.9207361, 175.5138259667, \"2/314\"],\n[-37.9562101, 175.5616106, \"972\"],\n[-37.9204318667, 175.5155629167, \"1/319\"],\n[-37.9203768167, 175.5151516333, \"2/319\"],\n[-37.91788475, 175.51599185, \"3/319\"],\n[-37.9208983667, 175.5150343667, \"322\"],\n[-37.9207446167, 175.5174052333, \"355\"],\n[-37.9213706667, 175.5182903833, \"364\"],\n[-37.9209673667, 175.5228385667, \"381\"],\n[-37.9121427, 175.48111095, \"1\"],\n[-37.9210744167, 175.526721, \"398\"],\n[-37.92060065, 175.5267811333, \"421\"],\n[-37.9203042167, 175.53031625, \"441\"],\n[-37.9205438667, 175.5322192667, \"472\"],\n[-37.95440025, 175.5603198167, \"949\"],\n[-37.9549745833, 175.5610077, \"955\"],\n[-37.9557622833, 175.5620862333, \"971\"],\n[-37.95699575, 175.56162575, \"976\"],\n[-37.96071475, 175.5671597833, \"1043\"],\n[-37.95945125, 175.5654647667, \"1048\"],\n[-37.9618459333, 175.56799375, \"1/1049\"],\n[-37.9616152833, 175.5678077833, \"2/1049\"],\n[-37.9661303667, 175.5708020833, \"2/1111\"],\n[-37.9667493333, 175.57131025, \"1115\"],\n[-37.96711415, 175.5715806667, \"1117\"],\n[-37.9675804833, 175.5719935, \"1121\"],\n[-37.9684296167, 175.5727105333, \"1133\"],\n[-37.9470393333, 175.5592361, \"859\"],\n[-37.91908795, 175.5034454, \"1/222\"],\n[-37.9190107333, 175.5030951833, \"2/222\"],\n[-37.9182063667, 175.4975724833, \"162\"],\n[-37.9215276333, 175.5398657167, \"338\"],\n[-37.9214903, 175.5403513167, \"347\"],\n[-37.9192073833, 175.5385113667, \"60\"],\n[-37.9194124167, 175.53863005, \"70\"],\n[-37.9202965333, 175.5390272333, \"170\"],\n[-37.9202201333, 175.5394256333, \"187\"],\n[-37.9200427667, 175.5393355, \"165\"],\n[-37.9196745333, 175.5387265333, \"110\"],\n[-37.9196443833, 175.5391287333, \"113\"],\n[-37.9200210333, 175.5388776, \"148\"],\n[-37.9198508167, 175.5392385667, \"139\"],\n[-37.9198443833, 175.53880195, \"130\"],\n[-37.92078415, 175.5393372, \"240\"],\n[-37.9203728, 175.5395211833, \"205\"],\n[-37.92054995, 175.5391807, \"218\"],\n[-37.9207016833, 175.53928345, \"222\"],\n[-37.9204983167, 175.5396219333, \"225\"],\n[-37.9206377, 175.5392380167, \"220\"],\n[-37.9209028833, 175.53941765, \"1/258\"],\n[-37.9206256667, 175.5397483667, \"243\"],\n[-37.92098855, 175.5394799, \"2/258\"],\n[-37.9211211, 175.54010905, \"303\"],\n[-37.9207808, 175.5398586, \"261\"],\n[-37.9209433167, 175.5399736333, \"285\"],\n[-37.9210710833, 175.53953355, \"3/258\"],\n[-37.9213220667, 175.54020665, \"325\"],\n[-37.9213740167, 175.5397642, \"320\"],\n[-37.9216746167, 175.5399703333, \"356\"],\n[-37.9217735667, 175.5401128333, \"370\"],\n[-37.9216458, 175.5405448833, \"373\"],\n[-37.92187255, 175.54025525, \"386\"],\n[-37.9219284333, 175.5408441667, \"401\"],\n[-37.9103304833, 175.4721328333, \"10\"],\n[-37.9102787167, 175.4719914, \"11\"],\n[-37.9108774833, 175.4715148, \"1\"],\n[-37.91067615, 175.4716186, \"3\"],\n[-37.9107711833, 175.4719261333, \"4\"],\n[-37.9105010667, 175.4716637167, \"5\"],\n[-37.9106344833, 175.4720128, \"6\"],\n[-37.9103480333, 175.4717653333, \"7\"],\n[-37.91015655, 175.4718258167, \"9\"],\n[-37.9104869167, 175.47212505, \"8\"],\n[-37.8164228, 175.5139775667, \"1\"],\n[-37.8062942167, 175.5133165, \"174\"],\n[-37.8153860333, 175.5132536167, \"21\"],\n[-37.8101489667, 175.5102431, \"70\"],\n[-37.81422025, 175.5124331667, \"36\"],\n[-37.8662685833, 175.38685585, \"101\"],\n[-37.8696932667, 175.3909689667, \"156\"],\n[-37.86591225, 175.38439815, \"82\"],\n[-37.8665148, 175.3851989333, \"100\"],\n[-37.8672423833, 175.385944, \"102\"],\n[-37.8633081833, 175.3778155667, \"18\"],\n[-37.8628883333, 175.3783555833, \"20\"],\n[-37.8633668167, 175.3794526667, \"34\"],\n[-37.8687968833, 175.3875604333, \"124\"],\n[-37.87375745, 175.4073473, \"309\"],\n[-37.871102, 175.3963777667, \"206\"],\n[-37.8643377167, 175.3813198167, \"48A\"],\n[-37.8647317667, 175.38235055, \"62\"],\n[-37.8653946, 175.3839015333, \"72\"],\n[-37.8691804167, 175.3885183, \"136\"],\n[-37.8695428667, 175.3900536333, \"150\"],\n[-37.8708532667, 175.3954820167, \"200\"],\n[-37.8696376833, 175.3931483, \"165\"],\n[-37.8708364167, 175.3980991167, \"221\"],\n[-37.8769260167, 175.3983096333, \"256B\"],\n[-37.8739190333, 175.4060778667, \"298\"],\n[-37.8736307833, 175.4070128, \"307\"],\n[-37.8743770667, 175.4077305, \"316\"],\n[-37.8703100667, 175.3894640833, \"146\"],\n[-37.8697010667, 175.39052375, \"154\"],\n[-37.8641413167, 175.3808931667, \"48E\"],\n[-37.8673080333, 175.3779883667, \"48B\"],\n[-37.8673718333, 175.3785550833, \"48C\"],\n[-37.8674977833, 175.3793694333, \"48D\"],\n[-37.86895095, 175.3799414833, \"66B\"],\n[-37.8626150667, 175.3776425167, \"14\"],\n[-37.87166565, 175.3986665667, \"254A\"],\n[-37.8723313833, 175.4007501, \"256A\"],\n[-37.8728116833, 175.4024396333, \"276A\"],\n[-37.8735445333, 175.4044031333, \"276B\"],\n[-37.8788183833, 175.3998753, \"276C\"],\n[-37.8700288, 175.39251035, \"170\"],\n[-37.8700618833, 175.3926480833, \"172\"],\n[-37.87032565, 175.3935790167, \"182\"],\n[-37.87129395, 175.3971019667, \"210\"],\n[-37.8684687167, 175.3787688, \"66A\"],\n[-37.86917345, 175.3816180667, \"66\"],\n[-37.8994036333, 175.46781995, \"2\"],\n[-37.89908445, 175.46789945, \"3\"],\n[-37.8992924, 175.46819555, \"4\"],\n[-37.8990193667, 175.4681169167, \"5\"],\n[-37.8994638333, 175.46840135, \"6\"],\n[-37.8988995167, 175.4683006333, \"7\"],\n[-37.8992033, 175.46852295, \"8\"],\n[-37.8866583833, 175.4532039667, \"33F\"],\n[-37.8864233833, 175.4521969667, \"32\"],\n[-37.8866203333, 175.4534927833, \"33E\"],\n[-37.88746175, 175.45179065, \"24A\"],\n[-37.8826686, 175.4521089833, \"48\"],\n[-37.8871881167, 175.4517683, \"26A\"],\n[-37.8891620833, 175.4522299333, \"2A\"],\n[-37.8870288833, 175.4517563333, \"28A\"],\n[-37.8847421333, 175.4521349667, \"46\"],\n[-37.8866198667, 175.4517700333, \"30A\"],\n[-37.8847517, 175.4530716833, \"49\"],\n[-37.8864979167, 175.4517625333, \"32A\"],\n[-37.8877413167, 175.4522157, \"22\"],\n[-37.8875273, 175.4522017833, \"24\"],\n[-37.8886485, 175.4522158667, \"10\"],\n[-37.8863276167, 175.4526999167, \"39\"],\n[-37.8872459167, 175.4521917333, \"26\"],\n[-37.8869694333, 175.4521850167, \"28\"],\n[-37.8884949833, 175.4526693667, \"11\"],\n[-37.8884365167, 175.4522167833, \"12\"],\n[-37.8883604667, 175.45266065, \"13\"],\n[-37.88909265, 175.4527732833, \"1A\"],\n[-37.8890918, 175.4526509167, \"1\"],\n[-37.8892247667, 175.4522300833, \"2\"],\n[-37.8889543167, 175.4526548333, \"3\"],\n[-37.88878835, 175.4526679667, \"5\"],\n[-37.8879826333, 175.4526516667, \"17\"],\n[-37.8879732333, 175.4522717, \"18\"],\n[-37.8876465167, 175.4529919333, \"21\"],\n[-37.8874666333, 175.4526953333, \"23\"],\n[-37.88718005, 175.45269155, \"27\"],\n[-37.88667215, 175.4527091167, \"35\"],\n[-37.8865333667, 175.452713, \"37\"],\n[-37.8869211167, 175.4527025667, \"31\"],\n[-37.8889064667, 175.45167665, \"6\"],\n[-37.8887104, 175.4530365333, \"7\"],\n[-37.8888331333, 175.45221635, \"8\"],\n[-37.8886320167, 175.4530411167, \"9\"],\n[-37.8866415, 175.4537552833, \"33D\"],\n[-37.8868032, 175.4537838167, \"33C\"],\n[-37.8868312167, 175.45354015, \"33B\"],\n[-37.88685175, 175.4532279333, \"33A\"],\n[-37.8867126, 175.4521915667, \"30\"],\n[-37.89138475, 175.4647044, \"31\"],\n[-37.8884147667, 175.4636648, \"60B\"],\n[-37.8944636333, 175.4644644833, \"10A\"],\n[-37.8944416833, 175.4645756833, \"10\"],\n[-37.8941219, 175.46491495, \"11\"],\n[-37.8943222167, 175.4642833333, \"12A\"],\n[-37.8930367667, 175.4648168, \"15A\"],\n[-37.8931255, 175.4648136167, \"15B\"],\n[-37.8931035167, 175.4644127333, \"18\"],\n[-37.89292315, 175.4648254167, \"19\"],\n[-37.8942371, 175.4645572667, \"12\"],\n[-37.89397735, 175.4649136, \"13\"],\n[-37.89409845, 175.4643458667, \"14\"],\n[-37.89391375, 175.4645277333, \"16\"],\n[-37.8929680833, 175.4643951, \"20\"],\n[-37.8927348833, 175.46480795, \"21\"],\n[-37.8928157667, 175.46437875, \"22A\"],\n[-37.8927337833, 175.4643644833, \"22\"],\n[-37.8925561167, 175.46478985, \"23\"],\n[-37.8923608833, 175.4647155, \"25A\"],\n[-37.8924641833, 175.46493565, \"25\"],\n[-37.8949511, 175.4649854, \"1A\"],\n[-37.8922117333, 175.4647047167, \"27\"],\n[-37.8915681667, 175.4646986833, \"29\"],\n[-37.8913701667, 175.4642759667, \"30A-30D\"],\n[-37.8913990167, 175.4642620667, \"30\"],\n[-37.8913092333, 175.46425895, \"32\"],\n[-37.8912003667, 175.46469755, \"33\"],\n[-37.8911230333, 175.4642383333, \"34A\"],\n[-37.8911486667, 175.46399235, \"34B\"],\n[-37.8911019, 175.4642380167, \"34\"],\n[-37.89098895, 175.4646725333, \"35\"],\n[-37.89084255, 175.4642192333, \"36A\"],\n[-37.8909496167, 175.46422895, \"36B\"],\n[-37.8908606333, 175.4642211667, \"36\"],\n[-37.8948280833, 175.46498935, \"3\"],\n[-37.8908666333, 175.4646632833, \"37\"],\n[-37.8907759333, 175.4646549833, \"39A\"],\n[-37.8907216667, 175.4646473167, \"39B\"],\n[-37.8906578833, 175.4646450333, \"39C\"],\n[-37.8905375667, 175.4646237167, \"41\"],\n[-37.8904761833, 175.4646328667, \"43\"],\n[-37.8902941833, 175.4641657667, \"44A\"],\n[-37.8902406833, 175.46415805, \"44B\"],\n[-37.8903436667, 175.4641675667, \"44\"],\n[-37.89028625, 175.46460345, \"45\"],\n[-37.8901877167, 175.4641515667, \"46A\"],\n[-37.89013315, 175.4641537667, \"46B\"],\n[-37.8949605167, 175.4645740333, \"4\"],\n[-37.8900607167, 175.4645841333, \"47\"],\n[-37.8900176, 175.4641449833, \"48\"],\n[-37.8899419667, 175.464572, \"49A\"],\n[-37.88987475, 175.4645733833, \"49B\"],\n[-37.8898110667, 175.4645667333, \"49C\"],\n[-37.8896632167, 175.4645695333, \"49D\"],\n[-37.8898541333, 175.46475255, \"49E\"],\n[-37.8897567333, 175.4647629, \"49F\"],\n[-37.8896634667, 175.4647669667, \"49G\"],\n[-37.8898924167, 175.4641263833, \"50\"],\n[-37.8897574667, 175.4641326833, \"52\"],\n[-37.8889242333, 175.4640623167, \"54\"],\n[-37.88865535, 175.4644431167, \"55\"],\n[-37.8887946833, 175.4638054667, \"56A\"],\n[-37.8946296667, 175.4649645, \"5\"],\n[-37.8887812167, 175.4640600833, \"56\"],\n[-37.8884208167, 175.46442915, \"57\"],\n[-37.8886385667, 175.4637760833, \"58A\"],\n[-37.8886053667, 175.4640482, \"58\"],\n[-37.8882043667, 175.4644158333, \"59\"],\n[-37.8885028333, 175.463677, \"60A\"],\n[-37.88839355, 175.4640293333, \"60\"],\n[-37.8879652167, 175.4644074, \"61\"],\n[-37.8877446333, 175.46437685, \"63\"],\n[-37.8881502667, 175.4640036667, \"64\"],\n[-37.8879700167, 175.4639859333, \"66\"],\n[-37.8878033167, 175.4639686667, \"68\"],\n[-37.8947931167, 175.4646081167, \"6A\"],\n[-37.8948649333, 175.4643251333, \"6\"],\n[-37.8944518833, 175.4649523167, \"7\"],\n[-37.8946088333, 175.4645449667, \"8\"],\n[-37.89429465, 175.4649376, \"9\"],\n[-37.8951769667, 175.4645875167, \"2\"],\n[-37.88757625, 175.46390195, \"70\"],\n[-37.8940272667, 175.4652238, \"11A\"],\n[-37.8929316167, 175.4651907833, \"19A\"],\n[-37.8910516167, 175.4651865667, \"35A\"],\n[-37.8950131667, 175.4649995667, \"1\"],\n[-37.8946681667, 175.4642288167, \"8A\"],\n[-37.8950832, 175.4643562167, \"2A\"],\n[-37.89289445, 175.4639297667, \"1/22-11/22\"],\n[-37.8839810833, 175.4856866167, \"3\"],\n[-37.8839609167, 175.4862087667, \"4\"],\n[-37.8813706667, 175.4859932667, \"25\"],\n[-37.8815762, 175.4860593167, \"23\"],\n[-37.88176925, 175.48611695, \"21\"],\n[-37.8819291833, 175.4861440667, \"19\"],\n[-37.8812535167, 175.48626755, \"28\"],\n[-37.8814419833, 175.486341, \"26\"],\n[-37.8816454, 175.486407, \"24\"],\n[-37.8817836833, 175.4864573167, \"22\"],\n[-37.8823392667, 175.4866145167, \"20\"],\n[-37.8824710167, 175.4866385333, \"18\"],\n[-37.8823897, 175.4862189833, \"17\"],\n[-37.8810208833, 175.4857673833, \"27\"],\n[-37.8809241333, 175.4860744, \"32\"],\n[-37.8810838833, 175.4862027833, \"30\"],\n[-37.8807501667, 175.4855223, \"35\"],\n[-37.8809029, 175.48565165, \"33\"],\n[-37.88077235, 175.4859607667, \"34\"],\n[-37.8805942833, 175.48538215, \"37\"],\n[-37.8806738833, 175.48500715, \"39\"],\n[-37.8804429167, 175.4856392833, \"36\"],\n[-37.8806746667, 175.48474885, \"41\"],\n[-37.8802171333, 175.4854475, \"38\"],\n[-37.8800456167, 175.4853260167, \"40\"],\n[-37.8805906167, 175.4847991833, \"43\"],\n[-37.8803164833, 175.4846505, \"49\"],\n[-37.8803990833, 175.4851850833, \"45\"],\n[-37.8802597333, 175.4850626167, \"47\"],\n[-37.8800347333, 175.4849136333, \"53\"],\n[-37.88023585, 175.4846241, \"51\"],\n[-37.87911725, 175.4844298333, \"63\"],\n[-37.8793032, 175.4845327167, \"61\"],\n[-37.8794998, 175.48456245, \"59\"],\n[-37.8796728333, 175.48464225, \"57\"],\n[-37.8798386, 175.4844502667, \"55B\"],\n[-37.87985855, 175.4847564333, \"55\"],\n[-37.8798501167, 175.4851561333, \"42\"],\n[-37.8792051, 175.48470885, \"65\"],\n[-37.8796635, 175.4850343667, \"44\"],\n[-37.8834352333, 175.4858534667, \"5\"],\n[-37.8832857667, 175.4859341667, \"7\"],\n[-37.88359745, 175.4863355833, \"8\"],\n[-37.8830905833, 175.4860275333, \"9\"],\n[-37.88377265, 175.48624865, \"6\"],\n[-37.8826329333, 175.4866849167, \"16\"],\n[-37.8825409333, 175.4862091167, \"15\"],\n[-37.8827294333, 175.4862135, \"13\"],\n[-37.8834279333, 175.4864298333, \"10\"],\n[-37.8832412, 175.4865308833, \"12\"],\n[-37.883058, 175.4865957667, \"14\"],\n[-37.8828529667, 175.4861158333, \"11\"],\n[-37.8834473167, 175.3733817, \"7\"],\n[-37.8821477, 175.3739511333, \"17\"],\n[-37.8835727833, 175.3742963167, \"4\"],\n[-37.8829996167, 175.37376495, \"11\"],\n[-37.8799677333, 175.3744603833, \"49\"],\n[-37.8801634167, 175.37443835, \"43\"],\n[-37.8131783667, 175.38169875, \"10\"],\n[-37.8129868833, 175.3819189, \"8\"],\n[-37.8122474333, 175.3825065833, \"6C\"],\n[-37.8130850833, 175.3823078833, \"6A\"],\n[-37.8124561333, 175.38300715, \"6B\"],\n[-37.81348525, 175.3804658333, \"12\"],\n[-37.81168165, 175.3801314, \"16B\"],\n[-37.8110145667, 175.3801371167, \"16C\"],\n[-37.8106325333, 175.3808725167, \"16D\"],\n[-37.8105196, 175.3799175, \"16E\"],\n[-37.8134586333, 175.37986425, \"16A\"],\n[-37.8608434833, 175.4499011, \"43\"],\n[-37.8612150667, 175.44990305, \"45\"],\n[-37.8618689167, 175.4498981167, \"47\"],\n[-37.86239125, 175.4505662333, \"49\"],\n[-37.8626794667, 175.4498931333, \"51\"],\n[-37.86272035, 175.4492676833, \"53\"],\n[-37.86160045, 175.4492898833, \"55\"],\n[-37.8830900667, 175.48706325, \"2\"],\n[-37.8833400667, 175.4873493167, \"3\"],\n[-37.88330455, 175.4870247333, \"4\"],\n[-37.8835654833, 175.4872303, \"5\"],\n[-37.8835200667, 175.48693185, \"6\"],\n[-37.8837668167, 175.4872335833, \"7\"],\n[-37.88372005, 175.48686615, \"8\"],\n[-37.88385335, 175.4870479667, \"9\"],\n[-37.8831409167, 175.4873846167, \"1\"],\n[-37.8838673, 175.4868328833, \"10\"],\n[-37.88305215, 175.4682743333, \"2\"],\n[-37.8832460833, 175.4683237, \"3\"],\n[-37.8833727333, 175.4683497167, \"4\"],\n[-37.8833539333, 175.4684306333, \"5\"],\n[-37.8832353667, 175.4684359833, \"6\"],\n[-37.8356606333, 175.4259896, \"44\"],\n[-37.8361144333, 175.4254149667, \"39\"],\n[-37.8298011333, 175.4247086333, \"97\"],\n[-37.8328783, 175.4254824333, \"73\"],\n[-37.8326006333, 175.42427305, \"71\"],\n[-37.831938, 175.42577585, \"85\"],\n[-37.8296909667, 175.4253358667, \"98A\"],\n[-37.8304079333, 175.42640705, \"98B\"],\n[-37.9136688833, 175.47027315, \"11\"],\n[-37.9136547333, 175.47069685, \"10\"],\n[-37.9138316333, 175.4708094833, \"12\"],\n[-37.9137826667, 175.4703954167, \"16\"],\n[-37.91318695, 175.4705625333, \"1\"],\n[-37.9132175333, 175.4701367, \"3\"],\n[-37.9134621167, 175.47077345, \"4\"],\n[-37.9134043833, 175.47044285, \"5\"],\n[-37.9136208, 175.4710819333, \"6\"],\n[-37.91350565, 175.47029995, \"7\"],\n[-37.9137004667, 175.4710525667, \"8\"],\n[-37.9135834833, 175.4699601167, \"9\"],\n[-37.8994691333, 175.46256515, \"2\"],\n[-37.8995118667, 175.4628252833, \"2A\"],\n[-37.8995123, 175.4630041667, \"2B\"],\n[-37.8994579333, 175.4622181667, \"1\"],\n[-37.8974378833, 175.4617671667, \"28\"],\n[-37.8974075167, 175.4619900333, \"26\"],\n[-37.89761855, 175.4608725333, \"25\"],\n[-37.8977565167, 175.4606562, \"27\"],\n[-37.8979072833, 175.4605283333, \"29\"],\n[-37.89808565, 175.4603934333, \"31\"],\n[-37.8982804333, 175.46037375, \"33\"],\n[-37.89837375, 175.4602651333, \"35\"],\n[-37.89802395, 175.46003665, \"50\"],\n[-37.8978844167, 175.4601356167, \"48\"],\n[-37.8977532, 175.4602402333, \"46\"],\n[-37.8976160167, 175.4603286167, \"44\"],\n[-37.89819545, 175.4599255167, \"52\"],\n[-37.8974890167, 175.4604333667, \"42\"],\n[-37.8973825833, 175.46056285, \"40\"],\n[-37.8984307833, 175.4623306333, \"14\"],\n[-37.8993176, 175.46225845, \"3\"],\n[-37.8982570167, 175.4622604, \"16\"],\n[-37.89807915, 175.4621847167, \"18\"],\n[-37.8979096167, 175.46211465, \"20\"],\n[-37.8977295167, 175.4620442, \"22\"],\n[-37.8983868833, 175.4619344833, \"11\"],\n[-37.8982446, 175.4618734, \"13\"],\n[-37.8980852, 175.46182235, \"15\"],\n[-37.8979221167, 175.4617471667, \"17\"],\n[-37.8977388167, 175.4616313167, \"19\"],\n[-37.8975900833, 175.4619512333, \"24\"],\n[-37.8976432833, 175.4613693, \"21\"],\n[-37.8976362833, 175.4611184667, \"23\"],\n[-37.8973338667, 175.4610383, \"34\"],\n[-37.89734735, 175.46128405, \"32\"],\n[-37.8973671167, 175.4615326833, \"30\"],\n[-37.8983740833, 175.4600625, \"37\"],\n[-37.8973341333, 175.46074505, \"38\"],\n[-37.8971390333, 175.46087135, \"36\"],\n[-37.8986548667, 175.4624186833, \"12\"],\n[-37.89883265, 175.462497, \"10\"],\n[-37.8990063167, 175.46257255, \"8\"],\n[-37.8991676833, 175.4626289833, \"6\"],\n[-37.8993200333, 175.4626184667, \"4\"],\n[-37.8990043833, 175.4621885667, \"7\"],\n[-37.8988871333, 175.4621417, \"9\"],\n[-37.8991505333, 175.4622658, \"5\"],\n[-37.9839604, 175.45833975, \"21\"],\n[-37.9833674167, 175.4588416833, \"15\"],\n[-37.8359882667, 175.4393378167, \"84\"],\n[-37.81988195, 175.4304131333, \"303\"],\n[-37.8180250667, 175.4265244667, \"359\"],\n[-37.8183922667, 175.42604195, \"361\"],\n[-37.8176359333, 175.4250678167, \"377\"],\n[-37.8345467667, 175.4387839833, \"99\"],\n[-37.83385615, 175.4387189, \"107\"],\n[-37.80686185, 175.4025271167, \"621E\"],\n[-37.8072254833, 175.4031670833, \"621F\"],\n[-37.8157959333, 175.4221578, \"401B\"],\n[-37.8061875667, 175.401462, \"621A\"],\n[-37.8058821833, 175.4010980167, \"621B\"],\n[-37.81469335, 175.4231989, \"406\"],\n[-37.8174082667, 175.4176129667, \"413\"],\n[-37.8141161167, 175.4226544167, \"416\"],\n[-37.8138467167, 175.4215990833, \"419\"],\n[-37.8135359833, 175.4220777667, \"424\"],\n[-37.81227475, 175.4238289333, \"426\"],\n[-37.81351715, 175.4209686667, \"431\"],\n[-37.8344727, 175.43943635, \"100\"],\n[-37.8341482167, 175.4394138833, \"102\"],\n[-37.8335249, 175.4387248167, \"111\"],\n[-37.8332444167, 175.4386937167, \"113\"],\n[-37.8258899667, 175.4354216333, \"201\"],\n[-37.8207155167, 175.4310458, \"287\"],\n[-37.82205395, 175.4322644, \"273\"],\n[-37.82211345, 175.4333285833, \"268\"],\n[-37.8116728667, 175.4194537167, \"440A\"],\n[-37.8113214333, 175.4130010333, \"491A\"],\n[-37.8163042833, 175.4288074167, \"348B\"],\n[-37.81563165, 175.4286594, \"348A\"],\n[-37.8059561833, 175.41024335, \"566\"],\n[-37.8054795833, 175.3996853333, \"621D\"],\n[-37.8049338667, 175.4001146333, \"621C\"],\n[-37.8194743833, 175.42997115, \"315\"],\n[-37.8179959833, 175.4296346167, \"328\"],\n[-37.8184434333, 175.4287788333, \"327\"],\n[-37.8064376833, 175.406161, \"597B\"],\n[-37.8062919333, 175.4056468333, \"597C\"],\n[-37.8165707167, 175.4301424, \"336B\"],\n[-37.8172738, 175.4285947167, \"336A\"],\n[-37.8189469333, 175.4303026167, \"312A\"],\n[-37.81940555, 175.4307701833, \"312B\"],\n[-37.8199143667, 175.4312459, \"300\"],\n[-37.8175767667, 175.4291582, \"334\"],\n[-37.8152067667, 175.4162956667, \"441B\"],\n[-37.8142325667, 175.4148164667, \"441D\"],\n[-37.8149509, 175.4159546667, \"441C\"],\n[-37.815999, 175.4175927333, \"441A\"],\n[-37.80413515, 175.4042530167, \"612\"],\n[-37.8036285, 175.4024796333, \"627B\"],\n[-37.8014316833, 175.4040596333, \"636\"],\n[-37.8026355333, 175.4024570667, \"638\"],\n[-37.8046866167, 175.4038756333, \"613\"],\n[-37.8028006833, 175.4016926167, \"639A\"],\n[-37.8015622167, 175.4017531, \"652\"],\n[-37.8004875333, 175.4008727667, \"670\"],\n[-37.7997891, 175.40002805, \"676\"],\n[-37.8083442333, 175.4139592833, \"509\"],\n[-37.8074935333, 175.4139840667, \"532\"],\n[-37.8072659167, 175.4124669333, \"543\"],\n[-37.8081279, 175.40823995, \"567A\"],\n[-37.8087099667, 175.4071535667, \"567C\"],\n[-37.804962, 175.4071512, \"586\"],\n[-37.8046189167, 175.4061273333, \"598\"],\n[-37.8052894167, 175.4083249, \"576\"],\n[-37.8060052167, 175.4083508333, \"571\"],\n[-37.8053679167, 175.4062368, \"597A\"],\n[-37.8051049667, 175.4051947167, \"607\"],\n[-37.8089437667, 175.4076931167, \"567B\"],\n[-37.8113679833, 175.4200005667, \"440B\"],\n[-37.8112042667, 175.4175891, \"463\"],\n[-37.8114033, 175.4191389167, \"442\"],\n[-37.81667955, 175.4240415167, \"387\"],\n[-37.8160871, 175.4248795, \"390\"],\n[-37.81503135, 175.4226562, \"401A\"],\n[-37.8158879667, 175.42335465, \"393\"],\n[-37.8104159333, 175.41701115, \"477\"],\n[-37.84176295, 175.4387852167, \"25\"],\n[-37.8394856333, 175.43871765, \"53\"],\n[-37.84315575, 175.4396268667, \"4\"],\n[-37.8382764833, 175.4387401333, \"65\"],\n[-37.8365486833, 175.4387233, \"81\"],\n[-37.83550145, 175.4387130167, \"91\"],\n[-37.8063373333, 175.40984385, \"565A\"],\n[-37.8068884167, 175.4101122333, \"553\"],\n[-37.8073266833, 175.40938595, \"565B\"],\n[-37.8022880667, 175.4015246833, \"647\"],\n[-37.8032717167, 175.3994722333, \"649\"],\n[-37.8125184833, 175.4206463, \"432A\"],\n[-37.8131087167, 175.4214898833, \"432D\"],\n[-37.81219155, 175.4220903333, \"432B\"],\n[-37.8123494833, 175.4226102667, \"432C\"],\n[-37.8323999167, 175.4388484167, \"113/1\"],\n[-37.8137884667, 175.4184721333, \"439B\"],\n[-37.8128705833, 175.4202196333, \"439A\"],\n[-37.8030845, 175.4029651, \"634\"],\n[-37.80229035, 175.4038689333, \"634A\"],\n[-37.8016416, 175.4044987167, \"634B\"],\n[-37.8040659833, 175.4028728333, \"627A\"],\n[-37.8039385667, 175.4012164167, \"627C\"],\n[-37.8041331833, 175.4011953333, \"627D\"],\n[-37.8104539833, 175.4114936333, \"501\"],\n[-37.8095276, 175.41551535, \"491C\"],\n[-37.8104942333, 175.4142607333, \"491B\"],\n[-37.8118822333, 175.4251615167, \"426A\"],\n[-37.9146426833, 175.46333945, \"8\"],\n[-37.9146966667, 175.4641698333, \"12\"],\n[-37.9148385667, 175.4646229167, \"14\"],\n[-37.9145846833, 175.4637018, \"10\"],\n[-37.9148759167, 175.4632119667, \"6\"],\n[-37.9150353667, 175.4631399, \"4\"],\n[-37.8791983833, 175.48491125, \"1\"],\n[-37.8794456833, 175.4852828333, \"4\"],\n[-37.8795405, 175.4855742333, \"6\"],\n[-37.8796772167, 175.4856965, \"8\"],\n[-37.87980185, 175.4857916833, \"10\"],\n[-37.8796864, 175.4861288, \"15\"],\n[-37.8797264167, 175.4859489333, \"12\"],\n[-37.8795614, 175.4860496167, \"13\"],\n[-37.8794049667, 175.4858653167, \"11\"],\n[-37.8792908167, 175.4856798667, \"9\"],\n[-37.8791893, 175.4854042167, \"5\"],\n[-37.879194, 175.4855697, \"7\"],\n[-37.87919295, 175.4851510333, \"3\"],\n[-37.9021503333, 175.4819923833, \"1\"],\n[-37.9020893333, 175.4817469667, \"2\"],\n[-37.90202325, 175.4814764333, \"3\"],\n[-37.90189885, 175.4813368167, \"4\"],\n[-37.9017681, 175.4813746667, \"5\"],\n[-37.9017408667, 175.4815826667, \"6\"],\n[-37.9018523, 175.4818607167, \"7\"],\n[-37.90192475, 175.4821145, \"8\"],\n[-37.91226175, 175.46593575, \"11\"],\n[-37.9122654667, 175.4666392333, \"2\"],\n[-37.9124824333, 175.4664875, \"1\"],\n[-37.9126646667, 175.4661762167, \"3\"],\n[-37.9121691, 175.4663371, \"4\"],\n[-37.9126312167, 175.4661364333, \"5\"],\n[-37.9121084167, 175.46614215, \"6\"],\n[-37.91239705, 175.46620525, \"7\"],\n[-37.912165, 175.4659997167, \"8\"],\n[-37.91234125, 175.4659673167, \"9\"],\n[-37.8640527833, 175.4896800333, \"1/239\"],\n[-37.8637439833, 175.4893278333, \"2/239\"],\n[-37.8399393, 175.4691242833, \"563\"],\n[-37.8403040167, 175.4695932333, \"555\"],\n[-37.8626741167, 175.4890199667, \"250\"],\n[-37.86235545, 175.4877547333, \"263\"],\n[-37.8621190833, 175.4881775833, \"260\"],\n[-37.8697908333, 175.4976855333, \"2/143\"],\n[-37.8696097167, 175.50222845, \"109\"],\n[-37.8650994667, 175.4906942667, \"217\"],\n[-37.8587062667, 175.4842270833, \"308\"],\n[-37.8585393667, 175.4832368167, \"323\"],\n[-37.856982, 175.4821791333, \"337\"],\n[-37.8557220333, 175.4813075, \"357\"],\n[-37.87269675, 175.51109805, \"9\"],\n[-37.86818685, 175.4965807167, \"154\"],\n[-37.8391881333, 175.4691244833, \"570\"],\n[-37.8392424167, 175.4682793833, \"573\"],\n[-37.8384787167, 175.4682486667, \"580\"],\n[-37.8380699833, 175.4677177667, \"586\"],\n[-37.83591845, 175.4652008167, \"1/620\"],\n[-37.8356297, 175.4656015333, \"2/620\"],\n[-37.85443655, 175.48126465, \"362\"],\n[-37.8543465667, 175.4803770667, \"363\"],\n[-37.8533677, 175.4796417833, \"383\"],\n[-37.8400570333, 175.4693165167, \"559\"],\n[-37.8701502, 175.5066921333, \"58\"],\n[-37.8688167, 175.5031792667, \"94\"],\n[-37.8633276833, 175.48971255, \"240\"],\n[-37.84499075, 175.47397555, \"489\"],\n[-37.8390078667, 175.4675286, \"579\"],\n[-37.8615817667, 175.4874484667, \"272\"],\n[-37.8611261, 175.48692335, \"1/276\"],\n[-37.8609821, 175.48677485, \"2/276\"],\n[-37.8443942167, 175.4735167, \"491\"],\n[-37.84453425, 175.4741157833, \"490\"],\n[-37.8728439333, 175.5114111333, \"5\"],\n[-37.8479137833, 175.47588805, \"449\"],\n[-37.8694130667, 175.5004653167, \"118\"],\n[-37.8381128333, 175.4689194667, \"578\"],\n[-37.8387493167, 175.4686557833, \"576\"],\n[-37.834946, 175.4616395, \"648\"],\n[-37.8355279667, 175.4648596667, \"626\"],\n[-37.8352287, 175.46082495, \"670\"],\n[-37.84359945, 175.4735256167, \"500\"],\n[-37.8437138833, 175.4730056833, \"501\"],\n[-37.8433661667, 175.4727594167, \"511\"],\n[-37.8429640667, 175.4725448333, \"513\"],\n[-37.8424090833, 175.4720656833, \"517\"],\n[-37.8419261, 175.4716311333, \"529\"],\n[-37.84138045, 175.4707106667, \"535\"],\n[-37.8407243, 175.4700550167, \"551\"],\n[-37.84032605, 175.4702964, \"552\"],\n[-37.8397895, 175.46968935, \"562\"],\n[-37.83966945, 175.4688137833, \"567\"],\n[-37.8458821833, 175.4743360167, \"479\"],\n[-37.8523557667, 175.4795548667, \"394\"],\n[-37.8519909833, 175.4785619833, \"401\"],\n[-37.85128865, 175.4790768167, \"408\"],\n[-37.8505837, 175.4777973667, \"419\"],\n[-37.8494416833, 175.4767902167, \"429\"],\n[-37.8501747833, 175.478082, \"422\"],\n[-37.8487495333, 175.477303, \"446\"],\n[-37.8613924167, 175.4866768333, \"275\"],\n[-37.8612592667, 175.48651665, \"277\"],\n[-37.8606625, 175.4863779, \"280\"],\n[-37.8602587667, 175.4860168333, \"288\"],\n[-37.8662275, 175.4932509, \"192\"],\n[-37.8656668, 175.4925030333, \"206\"],\n[-37.8718011, 175.50936105, \"31\"],\n[-37.871327, 175.50837775, \"41\"],\n[-37.8481012833, 175.4755436333, \"455\"],\n[-37.8662202833, 175.4971715833, \"2/164\"],\n[-37.8665327833, 175.4968656167, \"1/164\"],\n[-37.87262005, 175.5101317, \"11\"],\n[-37.8407464167, 175.4693628, \"553\"],\n[-37.8687301, 175.50368985, \"92\"],\n[-37.8576066333, 175.4826215667, \"331\"],\n[-37.867679, 175.4952846833, \"164\"],\n[-37.8698948833, 175.4990379333, \"133\"],\n[-37.8639344, 175.49046205, \"230\"],\n[-37.8645916667, 175.4912248333, \"224\"],\n[-37.8710460833, 175.50036065, \"125\"],\n[-37.8694796667, 175.4976021667, \"1/143\"],\n[-37.8733131, 175.4691004333, \"3A\"],\n[-37.87406055, 175.4694125667, \"6A\"],\n[-37.8735341167, 175.469327, \"5\"],\n[-37.8735109667, 175.46960345, \"7\"],\n[-37.87351105, 175.4698807333, \"11\"],\n[-37.8735358667, 175.47009225, \"13\"],\n[-37.8736709667, 175.4702864167, \"17\"],\n[-37.8737941667, 175.47025885, \"16\"],\n[-37.8738211333, 175.4693453167, \"4\"],\n[-37.8737987667, 175.46968045, \"8\"],\n[-37.8738769167, 175.4699632, \"10\"],\n[-37.8739402833, 175.4701521167, \"12\"],\n[-37.8732443833, 175.4692419333, \"3B\"],\n[-37.8740679833, 175.4695514833, \"6B\"],\n[-37.8739830167, 175.4704122667, \"14\"],\n[-37.8735580667, 175.4702317, \"15\"],\n[-37.87328745, 175.4697687167, \"9\"],\n[-37.8738628167, 175.46909085, \"2\"],\n[-37.8735833, 175.4690007, \"1\"],\n[-37.9140601833, 175.4734438833, \"1\"],\n[-37.91414235, 175.4726753, \"7A\"],\n[-37.9148196333, 175.4729240333, \"4\"],\n[-37.9145959167, 175.4727449, \"5\"],\n[-37.9144294, 175.4728081667, \"6\"],\n[-37.9142862333, 175.4729454, \"7\"],\n[-37.9140804667, 175.4729594, \"8\"],\n[-37.91442545, 175.4732771, \"2A\"],\n[-37.9146292, 175.47320695, \"3\"],\n[-37.9142898833, 175.4733283333, \"2\"],\n[-37.8924708, 175.4694829667, \"3\"],\n[-37.8921866833, 175.4690726, \"4-6\"],\n[-37.8744290667, 175.4729675333, \"16\"],\n[-37.8747743333, 175.4729266667, \"17\"],\n[-37.8743306833, 175.4749814667, \"2\"],\n[-37.8743712167, 175.4747777167, \"4\"],\n[-37.8743979833, 175.4745281667, \"6\"],\n[-37.8744096167, 175.47429405, \"8\"],\n[-37.8744245167, 175.4740094333, \"10\"],\n[-37.8744545333, 175.4736160667, \"12\"],\n[-37.8742322333, 175.47382395, \"10A\"],\n[-37.87422675, 175.47369315, \"12A\"],\n[-37.8742394, 175.4730406667, \"16A\"],\n[-37.8742450667, 175.4731634667, \"14A\"],\n[-37.8744790667, 175.4732784833, \"14\"],\n[-37.8747754833, 175.4731559, \"15\"],\n[-37.8747613167, 175.4734085833, \"13\"],\n[-37.8747389, 175.4736529667, \"11\"],\n[-37.8747211167, 175.47387885, \"9\"],\n[-37.8747006167, 175.4741313, \"7\"],\n[-37.8746909667, 175.4743708167, \"5\"],\n[-37.8746798167, 175.4746769, \"3\"],\n[-37.8748711167, 175.4748117167, \"3A\"],\n[-37.8748328833, 175.4749142833, \"1A\"],\n[-37.874621, 175.4750391, \"1\"],\n[-37.8755351667, 175.4795168667, \"6A\"],\n[-37.8754681, 175.4798997667, \"6\"],\n[-37.8756842833, 175.4798735833, \"8\"],\n[-37.8756412167, 175.4794951833, \"8A\"],\n[-37.8758023167, 175.4803141667, \"5\"],\n[-37.8757598667, 175.4801058, \"7\"],\n[-37.8757471833, 175.4799721833, \"9\"],\n[-37.8755322, 175.4802420833, \"3\"],\n[-37.8753549833, 175.47993535, \"4\"],\n[-37.8817592, 175.4617109667, \"4A\"],\n[-37.8820907333, 175.4622059833, \"5\"],\n[-37.8815066, 175.4617161667, \"4B\"],\n[-37.8817534333, 175.4618948333, \"6A\"],\n[-37.8815026667, 175.46184895, \"6B\"],\n[-37.8815932167, 175.46217415, \"10A\"],\n[-37.8817450167, 175.46226225, \"10\"],\n[-37.8818722667, 175.4626555167, \"11\"],\n[-37.8817309, 175.4624690333, \"12\"],\n[-37.8821211333, 175.4617608833, \"1\"],\n[-37.8817646833, 175.4615252167, \"2A\"],\n[-37.8815099, 175.4615357667, \"2B\"],\n[-37.88209615, 175.4619084, \"3\"],\n[-37.8820657333, 175.4624910833, \"7\"],\n[-37.8817491333, 175.4620671833, \"8A\"],\n[-37.8814882667, 175.4620343, \"8B\"],\n[-37.8820256333, 175.4626954, \"9\"],\n[-37.9011387, 175.4845455833, \"11A\"],\n[-37.9004465667, 175.4842316167, \"6\"],\n[-37.9006268333, 175.4836419, \"1\"],\n[-37.9005414333, 175.4845609667, \"10\"],\n[-37.9009292833, 175.4847187667, \"11\"],\n[-37.90099175, 175.48493945, \"13\"],\n[-37.900608, 175.4848214333, \"14\"],\n[-37.9002908667, 175.4835936167, \"2\"],\n[-37.9003859333, 175.4839906667, \"4\"],\n[-37.9007548667, 175.4840674, \"5\"],\n[-37.9008090833, 175.4842714333, \"7\"],\n[-37.90027845, 175.4844867667, \"8\"],\n[-37.90087325, 175.4844819167, \"9\"],\n[-37.9010882333, 175.4852001, \"15\"],\n[-37.9007465833, 175.4850981333, \"16\"],\n[-37.9008384, 175.48536275, \"18\"],\n[-37.9009003833, 175.4836903667, \"3A\"],\n[-37.9006948, 175.48386545, \"3\"],\n[-37.8352114167, 175.3932620667, \"614\"],\n[-37.8358616, 175.3932373667, \"607\"],\n[-37.8527932167, 175.4481416, \"66\"],\n[-37.8535776833, 175.4477748333, \"59\"],\n[-37.8513053333, 175.4468875167, \"88\"],\n[-37.8367923167, 175.39439545, \"597\"],\n[-37.8356857333, 175.39511535, \"594\"],\n[-37.8338485, 175.3905351667, \"644\"],\n[-37.8342344833, 175.3914293833, \"636\"],\n[-37.8349352, 175.39110985, \"631\"],\n[-37.8340942167, 175.3894187167, \"647\"],\n[-37.8361288, 175.3943564333, \"603\"],\n[-37.835456, 175.3923533167, \"621\"],\n[-37.8316758833, 175.3851704333, \"690\"],\n[-37.82606465, 175.3729124, \"823A\"],\n[-37.8360247333, 175.40845075, \"472\"],\n[-37.8367576167, 175.4058547167, \"499\"],\n[-37.8307850333, 175.3833224167, \"716\"],\n[-37.8297911333, 175.3838338167, \"722A\"],\n[-37.82856305, 175.3853371, \"722B\"],\n[-37.8366530833, 175.4158296167, \"407\"],\n[-37.8343563333, 175.3899946667, \"645\"],\n[-37.83378605, 175.3887814833, \"649\"],\n[-37.8383438833, 175.4167502667, \"403A\"],\n[-37.8269138333, 175.3707719, \"823B\"],\n[-37.8262433167, 175.37126425, \"823C\"],\n[-37.8278735833, 175.3745076, \"791C\"],\n[-37.82803615, 175.3751116833, \"791B\"],\n[-37.8284354167, 175.3743400333, \"791D\"],\n[-37.8283312333, 175.3722626167, \"803H\"],\n[-37.8308135833, 175.3857800333, \"700A\"],\n[-37.83039575, 175.3861873667, \"700B\"],\n[-37.8297918, 175.3866039833, \"700C\"],\n[-37.8323622833, 175.3869592167, \"680A\"],\n[-37.8317271833, 175.3872918333, \"680B\"],\n[-37.8320544167, 175.3863078833, \"680C\"],\n[-37.8265114833, 175.3737262, \"811A\"],\n[-37.8268406667, 175.37269475, \"811B\"],\n[-37.8272920833, 175.3716203, \"811C\"],\n[-37.8537437333, 175.4489413667, \"62\"],\n[-37.83192005, 175.3841341833, \"695\"],\n[-37.8368657833, 175.4028889833, \"525\"],\n[-37.83376425, 175.3968886333, \"590B\"],\n[-37.8299130333, 175.3859233667, \"702C\"],\n[-37.8305810833, 175.38512625, \"702B\"],\n[-37.8360455333, 175.4081179333, \"474\"],\n[-37.8276760833, 175.3808082167, \"756C\"],\n[-37.8283299333, 175.3802092167, \"756B\"],\n[-37.85043345, 175.4461019, \"100\"],\n[-37.8297131833, 175.38286445, \"724A\"],\n[-37.82929325, 175.383521, \"724B\"],\n[-37.8369757, 175.3983033667, \"571\"],\n[-37.8376989333, 175.4166211, \"403B\"],\n[-37.8368311167, 175.4050104667, \"505\"],\n[-37.8412127667, 175.4050228833, \"491A\"],\n[-37.83852105, 175.4062947, \"491B\"],\n[-37.8338348833, 175.3934827333, \"622B\"],\n[-37.8330032, 175.3939467333, \"622C\"],\n[-37.8282068, 175.376844, \"775A\"],\n[-37.8295163167, 175.3755049667, \"775B\"],\n[-37.828404, 175.37718215, \"775C\"],\n[-37.8362426, 175.39711805, \"578\"],\n[-37.8361603, 175.39675875, \"580\"],\n[-37.8424267833, 175.4340512167, \"232\"],\n[-37.84114775, 175.4311295833, \"272\"],\n[-37.82872315, 175.3709866833, \"803A\"],\n[-37.8285930667, 175.3703004167, \"803B\"],\n[-37.8294768667, 175.3691337, \"803C\"],\n[-37.8292017, 175.36888125, \"803D\"],\n[-37.8285291333, 175.36580345, \"803E\"],\n[-37.8298995333, 175.3673114167, \"803F\"],\n[-37.8302568833, 175.36760835, \"803G\"],\n[-37.8267714167, 175.3742385167, \"807\"],\n[-37.83126665, 175.3844291833, \"702A\"],\n[-37.83016355, 175.3818740833, \"734\"],\n[-37.8296635167, 175.3807643167, \"742\"],\n[-37.82974665, 175.3798092, \"747\"],\n[-37.829114, 175.3799039833, \"752\"],\n[-37.8288072833, 175.3779985667, \"765\"],\n[-37.8278203667, 175.3760339333, \"789\"],\n[-37.8274609333, 175.3753562, \"791A\"],\n[-37.8320049, 175.39796425, \"592B\"],\n[-37.8304380667, 175.3993220667, \"592C\"],\n[-37.8323366667, 175.3982387333, \"592D\"],\n[-37.8332103167, 175.39666495, \"590A\"],\n[-37.8362232667, 175.4049506333, \"504\"],\n[-37.83689715, 175.4034307333, \"523\"],\n[-37.8362677, 175.4030851833, \"522\"],\n[-37.8363718333, 175.4003976167, \"548\"],\n[-37.836423, 175.3953457167, \"585\"],\n[-37.83596855, 175.3959414667, \"586\"],\n[-37.8392194, 175.4133888333, \"439\"],\n[-37.8367003667, 175.4102236167, \"461\"],\n[-37.83586315, 175.4121175333, \"442\"],\n[-37.8387438833, 175.4244664667, \"318\"],\n[-37.8382094333, 175.42266185, \"346\"],\n[-37.8366223667, 175.4173578167, \"396\"],\n[-37.8405799833, 175.4294097167, \"284\"],\n[-37.8409799, 175.4286117333, \"289\"],\n[-37.8404157, 175.4273133667, \"291\"],\n[-37.8448297, 175.4386074, \"188\"],\n[-37.8445996333, 175.4385307833, \"190\"],\n[-37.8445953833, 175.4381869167, \"192\"],\n[-37.8462362333, 175.4406972, \"162\"],\n[-37.8453950333, 175.4394750167, \"180\"],\n[-37.8561178167, 175.4522334, \"11\"],\n[-37.8560307, 175.4511923667, \"17\"],\n[-37.8361288, 175.4061807167, \"492\"],\n[-37.83609445, 175.40683275, \"490\"],\n[-37.8545275667, 175.44950565, \"40\"],\n[-37.8375230333, 175.4199872333, \"372\"],\n[-37.8388781167, 175.4226393667, \"343\"],\n[-37.83701825, 175.41844345, \"378\"],\n[-37.8371192833, 175.4187303167, \"376\"],\n[-37.8373785667, 175.4194973167, \"374\"],\n[-37.8367387167, 175.4085570167, \"473\"],\n[-37.8318677333, 175.3858444, \"688\"],\n[-37.8327873167, 175.3878064833, \"674\"],\n[-37.83304365, 175.3883507667, \"664\"],\n[-37.8366133333, 175.4130614667, \"427\"],\n[-37.8508733333, 175.44641935, \"94\"],\n[-37.8433085667, 175.4359371667, \"212\"],\n[-37.8376679667, 175.3994774833, \"549\"],\n[-37.8388004167, 175.3998009833, \"549C\"],\n[-37.8369648167, 175.40033875, \"549B\"],\n[-37.8376661833, 175.3999770333, \"549A\"],\n[-37.8368055667, 175.3968196667, \"575\"],\n[-37.8369513667, 175.39729965, \"573\"],\n[-37.836328, 175.3975344167, \"576\"],\n[-37.8419685667, 175.4332863667, \"240\"],\n[-37.83147865, 175.3703224333, \"781A\"],\n[-37.8302962167, 175.3727004833, \"781B\"],\n[-37.8300511833, 175.3734676833, \"781C\"],\n[-37.9155750333, 175.4788872333, \"103\"],\n[-37.9080710333, 175.4815503833, \"39C\"],\n[-37.9081354167, 175.48171275, \"39B\"],\n[-37.90819845, 175.4819203833, \"39A\"],\n[-37.9165609667, 175.4784770333, \"115\"],\n[-37.91497395, 175.4792572167, \"99\"],\n[-37.9096164833, 175.4817164167, \"49\"],\n[-37.9116786667, 175.48074935, \"69\"],\n[-37.9140226, 175.479658, \"85\"],\n[-37.9069150833, 175.4829900333, \"27A\"],\n[-37.9069469833, 175.4828211667, \"27B\"],\n[-37.9141799333, 175.4796010667, \"87\"],\n[-37.9148269, 175.4793132333, \"95\"],\n[-37.9148061667, 175.47892315, \"97\"],\n[-37.9052287833, 175.4834456667, \"7A\"],\n[-37.9157397167, 175.4785532167, \"107A\"],\n[-37.9164115167, 175.4783404167, \"113\"],\n[-37.9162113333, 175.4786140333, \"109\"],\n[-37.9159243667, 175.4787397833, \"107B\"],\n[-37.91564195, 175.4783057333, \"107\"],\n[-37.9095836167, 175.48142755, \"49A\"],\n[-37.9097563, 175.4813824667, \"51A\"],\n[-37.9142505167, 175.47917545, \"89A\"],\n[-37.9143203667, 175.4795241, \"89\"],\n[-37.9143582167, 175.4791171833, \"91A\"],\n[-37.90809605, 175.4820482833, \"37A\"],\n[-37.9080426833, 175.4818123, \"37B\"],\n[-37.9079928333, 175.4816048, \"37C\"],\n[-37.9144982167, 175.4794426833, \"91\"],\n[-37.9146732833, 175.4793797, \"93\"],\n[-37.9046052333, 175.48406395, \"1\"],\n[-37.90479665, 175.4839646833, \"3\"],\n[-37.9049845333, 175.4838759833, \"5\"],\n[-37.9061106, 175.4833581667, \"17\"],\n[-37.9062431667, 175.4833112333, \"19\"],\n[-37.9063920333, 175.4832255333, \"21\"],\n[-37.9065708, 175.48314765, \"23\"],\n[-37.9067455333, 175.4830679333, \"25\"],\n[-37.90686575, 175.4825972667, \"29\"],\n[-37.9053759667, 175.4837128167, \"11\"],\n[-37.9055325, 175.4836456167, \"13\"],\n[-37.9057146333, 175.4835634667, \"15\"],\n[-37.9080300333, 175.4824985, \"35\"],\n[-37.9081798333, 175.4824187833, \"37\"],\n[-37.90842625, 175.4822687667, \"39\"],\n[-37.90871305, 175.4821525333, \"43\"],\n[-37.9071454167, 175.4828962333, \"31\"],\n[-37.90945405, 175.4818068167, \"47\"],\n[-37.9097787167, 175.4816407833, \"51\"],\n[-37.9101221667, 175.48147745, \"55\"],\n[-37.9099619333, 175.4815613833, \"53\"],\n[-37.9108703, 175.48113905, \"57\"],\n[-37.9111010167, 175.4810227833, \"61\"],\n[-37.9113033333, 175.4806476167, \"63A\"],\n[-37.9113063333, 175.4809305333, \"63\"],\n[-37.9113486333, 175.48054435, \"65\"],\n[-37.9115162333, 175.4808384167, \"67\"],\n[-37.9118321, 175.4806828667, \"71\"],\n[-37.912005, 175.4805931333, \"73\"],\n[-37.9126924, 175.4801856167, \"75\"],\n[-37.9128904333, 175.48013955, \"77\"],\n[-37.91306105, 175.4801038667, \"79\"],\n[-37.91323275, 175.4800307667, \"81\"],\n[-37.90504305, 175.48350075, \"7\"],\n[-37.9161411, 175.4791161, \"104\"],\n[-37.9052017167, 175.4837928833, \"9\"],\n[-37.9048593333, 175.4837002833, \"5A\"],\n[-37.9134008167, 175.4799760333, \"83\"],\n[-37.91608025, 175.4781871, \"111A\"],\n[-37.9159769333, 175.4778862, \"111B\"],\n[-37.91609255, 175.4778227833, \"111C\"],\n[-37.9162477333, 175.4781089667, \"111D\"],\n[-37.8954434833, 175.47689195, \"1\"],\n[-37.7968509833, 175.4409205333, \"218\"],\n[-37.7967954, 175.4415974333, \"226\"],\n[-37.79061135, 175.4311808833, \"105\"],\n[-37.7936930167, 175.4323678167, \"144\"],\n[-37.7936793, 175.4341171833, \"169\"],\n[-37.7906754167, 175.4300550333, \"101\"],\n[-37.7923387667, 175.4305234833, \"114\"],\n[-37.7921506667, 175.4312033333, \"119\"],\n[-37.79638115, 175.44350555, \"244\"],\n[-37.9026378, 175.4249371167, \"6\"],\n[-37.9018287167, 175.4251090167, \"15\"],\n[-37.9005140167, 175.4267575167, \"32\"],\n[-37.90030005, 175.4272810167, \"36\"],\n[-37.9000367833, 175.4266413167, \"1/45\"],\n[-37.8999172167, 175.4269248667, \"2/45\"],\n[-37.8998128167, 175.42722285, \"3/45\"],\n[-37.8997664167, 175.4278138333, \"46\"],\n[-37.89874045, 175.42933485, \"47\"],\n[-37.90246715, 175.4251130833, \"8\"],\n[-37.98415945, 175.5440391833, \"1/398\"],\n[-37.9842023, 175.5446759833, \"398\"],\n[-37.9673053833, 175.5493618, \"1/200\"],\n[-37.9595488167, 175.5503643667, \"2/94\"],\n[-37.9548066833, 175.5547442167, \"41\"],\n[-37.975043, 175.5502804667, \"288\"],\n[-37.9674483333, 175.54948405, \"2/200\"],\n[-37.9675936167, 175.54960055, \"3/200\"],\n[-37.9536896833, 175.5558200833, \"21\"],\n[-37.9537523333, 175.5551846667, \"24\"],\n[-37.9758721, 175.55151395, \"297\"],\n[-37.9715911333, 175.5505416, \"246\"],\n[-37.97615305, 175.5531409833, \"1/299\"],\n[-37.9759959833, 175.5531752167, \"2/299\"],\n[-37.9762314333, 175.5517778833, \"3/299\"],\n[-37.9789959667, 175.5511051167, \"1/317\"],\n[-37.9791978667, 175.5509703167, \"2/317\"],\n[-37.97016405, 175.5511682333, \"233\"],\n[-37.9840100667, 175.5422511833, \"3/398\"],\n[-37.9832081667, 175.5409176, \"4/398\"],\n[-37.9626150333, 175.5485202333, \"1/142\"],\n[-37.9620708667, 175.5493090333, \"139\"],\n[-37.9646945333, 175.5484742, \"164\"],\n[-37.95884865, 175.5515776333, \"91\"],\n[-37.9588039667, 175.5510064, \"1/94\"],\n[-37.9794108833, 175.5497014333, \"332\"],\n[-37.9806783167, 175.5477771, \"352\"],\n[-37.8757744167, 175.46972585, \"1\"],\n[-37.8758043667, 175.4700843, \"3\"],\n[-37.8758559833, 175.4703261667, \"5\"],\n[-37.8759855667, 175.4703921667, \"7\"],\n[-37.8761207333, 175.4703036833, \"9\"],\n[-37.8763731833, 175.4703581, \"8B\"],\n[-37.8763879, 175.4701746167, \"8A\"],\n[-37.8760971833, 175.4699428, \"6\"],\n[-37.8763947, 175.46978015, \"4\"],\n[-37.8760231167, 175.4695721, \"2\"],\n[-37.84887105, 175.5612401333, \"268\"],\n[-37.8480716667, 175.5605471, \"1/271\"],\n[-37.8481743833, 175.5599216, \"2/271\"],\n[-37.8475651333, 175.5602482833, \"273\"],\n[-37.8549548833, 175.5645155167, \"187\"],\n[-37.8590531333, 175.56929455, \"111\"],\n[-37.8579502, 175.5697828667, \"1/128\"],\n[-37.8577433333, 175.5697812167, \"2/128\"],\n[-37.8663423167, 175.5687757833, \"25\"],\n[-37.8659378833, 175.5701593333, \"1/32\"],\n[-37.8656144667, 175.5703594167, \"2/32\"],\n[-37.9075306667, 175.4735409, \"8\"],\n[-37.90741835, 175.4736250333, \"12\"],\n[-37.9077710167, 175.4736747, \"3\"],\n[-37.90756245, 175.4735306833, \"7\"],\n[-37.90776065, 175.4736345167, \"4\"],\n[-37.9076790833, 175.4734552333, \"6\"],\n[-37.90770165, 175.4734532167, \"5\"],\n[-37.90742555, 175.4736053833, \"11\"],\n[-37.9074725833, 175.4739170833, \"14\"],\n[-37.9074575333, 175.47388525, \"13\"],\n[-37.90752205, 175.4737226167, \"10\"],\n[-37.9075427667, 175.4737034667, \"9\"],\n[-37.9078406167, 175.4738848333, \"1\"],\n[-37.9078192667, 175.4738328667, \"2\"],\n[-37.9079175667, 175.4742687333, \"19\"],\n[-37.9076409333, 175.4739459, \"15\"],\n[-37.9077643333, 175.4742689, \"18\"],\n[-37.9077539833, 175.4742287, \"17\"],\n[-37.9076577333, 175.47400055, \"16\"],\n[-37.9079030333, 175.4742141667, \"20\"],\n[-37.9156256167, 175.4748749833, \"78\"],\n[-37.9146128333, 175.4721344167, \"56A\"],\n[-37.9151661833, 175.4733835167, \"66\"],\n[-37.9140138, 175.4694897, \"30\"],\n[-37.9140857167, 175.46969035, \"32\"],\n[-37.9144033, 175.4695158, \"33\"],\n[-37.91414165, 175.4698978167, \"34\"],\n[-37.91444875, 175.4697295833, \"35\"],\n[-37.9142653833, 175.4703260667, \"38\"],\n[-37.9142211667, 175.4701128667, \"36\"],\n[-37.9155440833, 175.4732913, \"65\"],\n[-37.9134283, 175.4674369333, \"14\"],\n[-37.9155934667, 175.4735220833, \"67\"],\n[-37.9151737667, 175.4721487333, \"57\"],\n[-37.9155787333, 175.4746912167, \"76\"],\n[-37.9143332167, 175.4693118167, \"29\"],\n[-37.9151155, 175.4718793167, \"55\"],\n[-37.9134783167, 175.4676862167, \"16\"],\n[-37.91350365, 175.4678403, \"18\"],\n[-37.9135371667, 175.4680652, \"20\"],\n[-37.9135827167, 175.4682467, \"22\"],\n[-37.9136676, 175.4684140667, \"24\"],\n[-37.91375, 175.4685284833, \"26\"],\n[-37.9144983333, 175.4699386333, \"39\"],\n[-37.9145579833, 175.4701247833, \"41\"],\n[-37.9146344333, 175.4703215833, \"43\"],\n[-37.9140692167, 175.4706085833, \"46\"],\n[-37.91469445, 175.47051275, \"47\"],\n[-37.9143359333, 175.4705744833, \"48\"],\n[-37.9147432833, 175.47069995, \"49\"],\n[-37.9146982, 175.4717894167, \"54\"],\n[-37.9147643333, 175.47204645, \"56\"],\n[-37.91485235, 175.4723157167, \"58\"],\n[-37.9149395167, 175.4725939833, \"60\"],\n[-37.9144000333, 175.4707812, \"50\"],\n[-37.9148043167, 175.4709096, \"51\"],\n[-37.9157513167, 175.4752532333, \"82\"],\n[-37.9158039333, 175.47545195, \"84\"],\n[-37.9156901667, 175.4750640167, \"80\"],\n[-37.9158563833, 175.4756482667, \"86\"],\n[-37.91508355, 175.4731087, \"64\"],\n[-37.9154807667, 175.4730812167, \"63\"],\n[-37.9154616667, 175.4742882833, \"72\"],\n[-37.91552175, 175.47449895, \"74\"],\n[-37.9790357333, 175.4624267667, \"19\"],\n[-37.9791358667, 175.4611471, \"6\"],\n[-37.9797030333, 175.4636025333, \"30\"],\n[-37.9790060167, 175.4621910833, \"17\"],\n[-37.9786417667, 175.4608476833, \"5\"],\n[-37.9804888333, 175.4691719, \"83\"],\n[-37.9793757333, 175.4644864667, \"37\"],\n[-37.9794654667, 175.4649678833, \"39\"],\n[-37.9815936, 175.4721696167, \"104\"],\n[-37.9806346333, 175.46753305, \"62\"],\n[-37.9801957667, 175.4694512667, \"1/83\"],\n[-37.9813943, 175.4789249667, \"2/172\"],\n[-37.9811831333, 175.4791219167, \"1/172\"],\n[-37.9815603333, 175.4787492833, \"3/172\"],\n[-37.9817079333, 175.4785843667, \"4/172\"],\n[-37.98098455, 175.4692836, \"84\"],\n[-37.9797661167, 175.4828635833, \"209\"],\n[-37.9819302167, 175.4751814833, \"130\"],\n[-37.9810331167, 175.4792768333, \"172\"],\n[-37.9791101833, 175.4807109667, \"193\"],\n[-37.9211995833, 175.54297615, \"118\"],\n[-37.9213446, 175.5421832, \"105\"],\n[-37.9211681167, 175.54216685, \"107\"],\n[-37.9209082667, 175.5425332333, \"111\"],\n[-37.9208103667, 175.542655, \"113\"],\n[-37.92117155, 175.5424739667, \"109\"],\n[-37.9206148667, 175.5429439167, \"119\"],\n[-37.9207782667, 175.5431801, \"117\"],\n[-37.9210005833, 175.542809, \"115\"],\n[-37.9223620333, 175.5419011, \"44\"],\n[-37.9221513667, 175.5414861833, \"22\"],\n[-37.9220122833, 175.54171955, \"48\"],\n[-37.9214541667, 175.5419499167, \"103\"],\n[-37.9216856333, 175.54226385, \"110\"],\n[-37.92188255, 175.54189345, \"70\"],\n[-37.9216037833, 175.5417236667, \"79\"],\n[-37.9217873667, 175.5420768833, \"90\"],\n[-37.9207292667, 175.5435594333, \"127\"],\n[-37.9215061833, 175.5424147833, \"112\"],\n[-37.9214096333, 175.5425683833, \"114\"],\n[-37.9212674167, 175.5427841667, \"116\"],\n[-37.9211160833, 175.5431952167, \"120\"],\n[-37.9210373167, 175.5434004167, \"122\"],\n[-37.9209575167, 175.5435602167, \"124\"],\n[-37.9206745667, 175.5433710667, \"125\"],\n[-37.92082745, 175.54361115, \"128\"],\n[-37.9209216, 175.5437402667, \"126\"],\n[-37.9205988167, 175.5431733333, \"123\"],\n[-37.9204654167, 175.54300295, \"121\"],\n[-37.9059849833, 175.4685779333, \"10\"],\n[-37.9063778167, 175.4688591, \"11\"],\n[-37.9057737333, 175.4693218333, \"2\"],\n[-37.9064842, 175.4693450167, \"7\"],\n[-37.9060801333, 175.46881255, \"12\"],\n[-37.9060230333, 175.46940395, \"1\"],\n[-37.9060816167, 175.4691977833, \"3\"],\n[-37.90581185, 175.4691003833, \"4\"],\n[-37.9063729333, 175.4693653667, \"5\"],\n[-37.9058748833, 175.4689197333, \"6\"],\n[-37.90586325, 175.46860955, \"8\"],\n[-37.9064269833, 175.4690855667, \"9\"],\n[-37.8955635667, 175.4714488333, \"3\"],\n[-37.89574055, 175.4716702667, \"6\"],\n[-37.8760291333, 175.4752209667, \"4\"],\n[-37.8761012667, 175.4756677333, \"5\"],\n[-37.8761172, 175.4749063833, \"6A\"],\n[-37.8762485667, 175.4748657167, \"6B\"],\n[-37.8765363167, 175.4750690167, \"10\"],\n[-37.87633255, 175.4751297667, \"8\"],\n[-37.8767972333, 175.4754502, \"13\"],\n[-37.8767630833, 175.4749863833, \"12\"],\n[-37.87581215, 175.4752698833, \"2\"],\n[-37.8757609167, 175.4757446833, \"1\"],\n[-37.8774312833, 175.4746756833, \"25\"],\n[-37.87757445, 175.4749078, \"23\"],\n[-37.8773315167, 175.4749104, \"21\"],\n[-37.8772196833, 175.4750822833, \"19\"],\n[-37.8771222167, 175.47521495, \"17\"],\n[-37.8769752, 175.4753515167, \"15\"],\n[-37.8766353, 175.47550835, \"11\"],\n[-37.8764627667, 175.4755687833, \"9\"],\n[-37.8762885333, 175.4756105, \"7\"],\n[-37.8759247333, 175.4757173667, \"3\"],\n[-37.8769794333, 175.4747672167, \"14\"],\n[-37.97891525, 175.4601520333, \"772\"],\n[-37.9464645333, 175.4822205167, \"329\"],\n[-37.9470361333, 175.4829135, \"337\"],\n[-37.9713491167, 175.4612835833, \"684\"],\n[-37.9615547167, 175.4636732, \"578\"],\n[-37.9642195, 175.4617228, \"606\"],\n[-37.9214648167, 175.4783231833, \"49\"],\n[-37.9212708833, 175.47781485, \"1/49\"],\n[-37.9230265, 175.478089, \"65\"],\n[-37.9669955167, 175.46143295, \"653\"],\n[-37.9801441, 175.4596936833, \"784\"],\n[-37.91909305, 175.4782088, \"27\"],\n[-37.9278700667, 175.4760814333, \"122\"],\n[-37.9393246667, 175.4798381833, \"256\"],\n[-37.98099395, 175.4594168167, \"788\"],\n[-37.9235198, 175.4765764833, \"76\"],\n[-37.9838248833, 175.4507092333, \"885\"],\n[-37.9840650667, 175.4492086167, \"893\"],\n[-37.9819271833, 175.4584667833, \"812\"],\n[-37.97680425, 175.4605180667, \"746\"],\n[-37.9772555, 175.4612509, \"751\"],\n[-37.9553357833, 175.4689185833, \"484\"],\n[-37.9549016, 175.4705773167, \"469\"],\n[-37.9549819, 175.4681984167, \"492\"],\n[-37.95610765, 175.4687586833, \"495\"],\n[-37.9558325167, 175.4681527833, \"496\"],\n[-37.9563786, 175.4673479667, \"506\"],\n[-37.9588897333, 175.4662872333, \"549\"],\n[-37.9374805167, 175.4792935833, \"232\"],\n[-37.9385204667, 175.4796240833, \"250\"],\n[-37.9418827167, 175.48053625, \"282\"],\n[-37.9422215667, 175.4814613667, \"291\"],\n[-37.9452781, 175.4815002, \"318\"],\n[-37.9482466667, 175.4790639833, \"372\"],\n[-37.9497215167, 175.47691875, \"396\"],\n[-37.9341688833, 175.4782591167, \"196\"],\n[-37.9265146833, 175.4765383667, \"109\"],\n[-37.93050445, 175.4778804333, \"151\"],\n[-37.9283181, 175.4771124167, \"129\"],\n[-37.9251162, 175.4762160667, \"98\"],\n[-37.92343355, 175.4771558167, \"73\"],\n[-37.9239441667, 175.4794231667, \"75\"],\n[-37.9241087667, 175.4769972833, \"83\"],\n[-37.9222823667, 175.4774759333, \"61\"],\n[-37.9199369167, 175.4774060333, \"34\"],\n[-37.9203899333, 175.475518, \"36\"],\n[-37.9203777167, 175.4773256, \"40\"],\n[-37.9206612167, 175.4765307833, \"42\"],\n[-37.9210448, 175.4771433833, \"44\"],\n[-37.9220389167, 175.47755095, \"57\"],\n[-37.91929535, 175.4775611167, \"26\"],\n[-37.9216718667, 175.4775874667, \"55\"],\n[-37.98317275, 175.4515617, \"874\"],\n[-37.9834542667, 175.449681, \"890\"],\n[-37.94112955, 175.48040145, \"278\"],\n[-37.9511382833, 175.4768400333, \"401\"],\n[-37.9529536833, 175.4734404167, \"447\"],\n[-37.9818127667, 175.4589793667, \"794\"],\n[-37.9216345833, 175.4771334167, \"50\"],\n[-37.9817718333, 175.4597500333, \"789\"],\n[-37.8840469167, 175.4595505333, \"3A\"],\n[-37.8836735833, 175.4604158333, \"4B\"],\n[-37.88403835, 175.4600429333, \"5\"],\n[-37.8834882, 175.4601379, \"1A\"],\n[-37.88367125, 175.4597893667, \"1\"],\n[-37.8835863, 175.4604458167, \"2A\"],\n[-37.8835968, 175.4601401333, \"2\"],\n[-37.88386135, 175.4597867, \"3\"],\n[-37.88376075, 175.4601527667, \"4\"],\n[-37.8838918833, 175.4601641667, \"6\"],\n[-37.8842726667, 175.4597851667, \"5A\"],\n[-37.8840725833, 175.4598049333, \"3B\"],\n[-37.8976647333, 175.4710761333, \"3\"],\n[-37.8975512333, 175.4708644667, \"7\"],\n[-37.8973241167, 175.4703941833, \"15\"],\n[-37.8972415667, 175.4702575667, \"17\"],\n[-37.8973903333, 175.4705850167, \"13\"],\n[-37.8974731667, 175.4707213, \"9\"],\n[-37.8976014, 175.4709784167, \"5\"],\n[-37.8973658833, 175.4713826333, \"1\"],\n[-37.88263055, 175.4789374667, \"2\"],\n[-37.88295785, 175.4787557167, \"3\"],\n[-37.88262705, 175.4787408333, \"4\"],\n[-37.8828655833, 175.47856665, \"5\"],\n[-37.8826968, 175.4785708667, \"6\"],\n[-37.8829381167, 175.4790952, \"1\"],\n[-37.8829405833, 175.4789477, \"1A\"],\n[-37.9471062833, 175.4443811, \"285\"],\n[-37.9482693333, 175.4618783667, \"129\"],\n[-37.9477489, 175.4616306833, \"134\"],\n[-37.94778825, 175.4623992333, \"120\"],\n[-37.9476318333, 175.4527723667, \"205\"],\n[-37.9487271, 175.47377555, \"22\"],\n[-37.9483278667, 175.4699560333, \"64\"],\n[-37.9505486833, 175.475507, \"3\"],\n[-37.9471956833, 175.45408385, \"192\"],\n[-37.9474878167, 175.4494432167, \"241\"],\n[-37.9465419167, 175.4494655333, \"240\"],\n[-37.8980432167, 175.4818800667, \"20\"],\n[-37.8980799167, 175.4813788167, \"11\"],\n[-37.8979071, 175.4814689833, \"11A\"],\n[-37.8976527, 175.4820540667, \"10\"],\n[-37.8978225833, 175.48198855, \"12\"],\n[-37.8980157833, 175.4823010833, \"14\"],\n[-37.8981133, 175.4825153333, \"18\"],\n[-37.8972657167, 175.48179755, \"1\"],\n[-37.8982001167, 175.4818174667, \"22\"],\n[-37.89831945, 175.48166055, \"26\"],\n[-37.8974300667, 175.4821601, \"2\"],\n[-37.8976037833, 175.4823142833, \"6\"],\n[-37.8975959167, 175.4816213167, \"7\"],\n[-37.8977532667, 175.48153515, \"9\"],\n[-37.8985379, 175.4825481, \"24\"],\n[-37.8966536667, 175.4826586833, \"25A\"],\n[-37.8984072, 175.48528765, \"54A\"],\n[-37.8960960167, 175.4812310167, \"5\"],\n[-37.8976620167, 175.4837889667, \"38\"],\n[-37.8957596833, 175.4808671833, \"1\"],\n[-37.896049, 175.4808353333, \"1A\"],\n[-37.89630705, 175.4808317833, \"3\"],\n[-37.8980094, 175.4834140167, \"34\"],\n[-37.89804835, 175.4835438167, \"36\"],\n[-37.8978337, 175.483244, \"30\"],\n[-37.8977234667, 175.4828271167, \"26\"],\n[-37.8974671333, 175.4832206833, \"28\"],\n[-37.89729555, 175.4826933167, \"22\"],\n[-37.8982734, 175.4859548167, \"60\"],\n[-37.8993387167, 175.4873577333, \"76\"],\n[-37.8971479167, 175.4808783833, \"10\"],\n[-37.8959557167, 175.48164725, \"11\"],\n[-37.8973392833, 175.4807942, \"12\"],\n[-37.8973916333, 175.4808756, \"14\"],\n[-37.8966921833, 175.4822478333, \"21\"],\n[-37.89642605, 175.4824713833, \"23\"],\n[-37.8967609167, 175.4825135833, \"25\"],\n[-37.8971820833, 175.4809704667, \"16\"],\n[-37.89693775, 175.4814169833, \"18A\"],\n[-37.8968612333, 175.4812093333, \"18\"],\n[-37.8968348, 175.4827666333, \"27\"],\n[-37.8969158833, 175.4829914167, \"29\"],\n[-37.8969838333, 175.4832029333, \"31\"],\n[-37.89760075, 175.4835397, \"32\"],\n[-37.8970568167, 175.48351615, \"33\"],\n[-37.8967365, 175.4807273, \"2\"],\n[-37.8979339167, 175.4848533, \"44\"],\n[-37.8967854, 175.4809565833, \"4\"],\n[-37.8958402333, 175.481278, \"7\"],\n[-37.8964383667, 175.4812642667, \"9\"],\n[-37.8980381167, 175.4851460667, \"48\"],\n[-37.8985301167, 175.4851492, \"52\"],\n[-37.8981529, 175.4855380167, \"54\"],\n[-37.8986448333, 175.4855899833, \"56\"],\n[-37.8985807833, 175.4870328333, \"66\"],\n[-37.8986987667, 175.4873079333, \"68\"],\n[-37.8987788167, 175.4875430667, \"74\"],\n[-37.89890085, 175.4879704667, \"80\"],\n[-37.8966231333, 175.4820502667, \"17A\"],\n[-37.8964502667, 175.4821239667, \"17B\"],\n[-37.896275, 175.48221365, \"17\"],\n[-37.8964864333, 175.4816180167, \"15\"],\n[-37.89829765, 175.4849233667, \"48A\"],\n[-37.8983503, 175.4862882, \"62\"],\n[-37.8963564333, 175.4817611, \"15A\"],\n[-37.8961816167, 175.4819250833, \"15B\"],\n[-37.8838324667, 175.4758307667, \"1\"],\n[-37.8836433833, 175.4758399333, \"2\"],\n[-37.8833977167, 175.47575535, \"3\"],\n[-37.8833314667, 175.4755896167, \"4\"],\n[-37.8834651, 175.4752073, \"5A\"],\n[-37.8833988833, 175.4754131, \"5\"],\n[-37.8836138833, 175.4754014333, \"6\"],\n[-37.88376315, 175.4754688833, \"7\"],\n[-37.8453489, 175.4520246, \"12\"],\n[-37.8453374333, 175.44998695, \"30\"],\n[-37.8456579, 175.4496963667, \"33\"],\n[-37.84527125, 175.4440318, \"40\"],\n[-37.8447595167, 175.4494073667, \"38\"],\n[-37.8453093333, 175.4489722667, \"40\"],\n[-37.8453195333, 175.4515149667, \"16\"],\n[-37.8833522167, 175.4662158833, \"2/13\"],\n[-37.8838213, 175.46524815, \"10A\"],\n[-37.88380855, 175.46613485, \"7A\"],\n[-37.8832769, 175.4662041167, \"1/13\"],\n[-37.88379275, 175.4654513833, \"10\"],\n[-37.88377815, 175.4658082667, \"11\"],\n[-37.88342395, 175.4662276833, \"3/13\"],\n[-37.883518, 175.4662264667, \"4/13\"],\n[-37.8836896667, 175.4656703833, \"14\"],\n[-37.8834471333, 175.4658730667, \"15\"],\n[-37.88344295, 175.46574735, \"16\"],\n[-37.8843819333, 175.4658316167, \"1\"],\n[-37.8844031667, 175.4655400167, \"2\"],\n[-37.8841441667, 175.4660546, \"3A\"],\n[-37.8841592333, 175.46611295, \"3B\"],\n[-37.8842008167, 175.4658206, \"3\"],\n[-37.8841960167, 175.4655438833, \"4\"],\n[-37.8840714, 175.4658066333, \"5\"],\n[-37.8840438, 175.4655440333, \"6\"],\n[-37.8839425167, 175.46582275, \"7\"],\n[-37.8839567167, 175.4654131167, \"8\"],\n[-37.8837516, 175.4660999833, \"9\"],\n[-37.883711, 175.4655393167, \"12\"],\n[-37.9413734667, 175.4995142833, \"306\"],\n[-37.9264553167, 175.4927426, \"101\"],\n[-37.9263942667, 175.495421, \"93\"],\n[-37.9367796667, 175.49396525, \"218\"],\n[-37.9306420333, 175.4927391167, \"147\"],\n[-37.9314740333, 175.4922045333, \"156\"],\n[-37.9346698333, 175.4939321167, \"195\"],\n[-37.9210469167, 175.49473535, \"45\"],\n[-37.9244517667, 175.4930819833, \"76\"],\n[-37.92482195, 175.4937379833, \"85\"],\n[-37.9278428667, 175.49133405, \"116\"],\n[-37.928097, 175.4920060333, \"117\"],\n[-37.9259406, 175.4921851333, \"98\"],\n[-37.9432639333, 175.5024320833, \"321\"],\n[-37.9478774167, 175.50590725, \"360\"],\n[-37.9006717, 175.3709368333, \"24\"],\n[-37.90793765, 175.3650150333, \"121\"],\n[-37.9056018833, 175.3671315333, \"89\"],\n[-37.9073516833, 175.3655189333, \"119\"],\n[-37.8986494833, 175.4611173667, \"1\"],\n[-37.8984617833, 175.4610480667, \"3\"],\n[-37.8983073667, 175.4609003333, \"5\"],\n[-37.8985554833, 175.4614893833, \"2\"],\n[-37.8983680333, 175.4614087, \"4\"],\n[-37.8981331333, 175.4608315333, \"7\"],\n[-37.8982140167, 175.4613434833, \"6\"],\n[-37.8979873167, 175.4611021667, \"10\"],\n[-37.8980203833, 175.4609355833, \"9\"],\n[-37.89806, 175.4612782333, \"8\"],\n[-37.9028616833, 175.4335225667, \"29\"],\n[-37.9031164833, 175.4331062, \"27\"],\n[-37.9017570667, 175.4336130833, \"43\"],\n[-37.9018962667, 175.4342030167, \"42\"],\n[-37.9035645333, 175.4334544, \"21\"],\n[-37.8997075167, 175.4313605167, \"63\"],\n[-37.9028940667, 175.4339966333, \"28\"],\n[-37.90126025, 175.43430315, \"54\"],\n[-37.9009420667, 175.4335351333, \"57\"],\n[-37.8997543667, 175.4328163667, \"67\"],\n[-37.9042664833, 175.4334384833, \"9\"],\n[-37.9004958, 175.4365008, \"56\"],\n[-37.8999357667, 175.4364970167, \"58\"],\n[-37.9193787167, 175.4035627667, \"29\"],\n[-37.9187184, 175.40134295, \"50\"],\n[-37.91970595, 175.4043742667, \"21\"],\n[-37.93012645, 175.3979479333, \"179\"],\n[-37.9287776167, 175.3984838, \"167\"],\n[-37.9244404833, 175.40003235, \"125\"],\n[-37.9182969, 175.4015440667, \"46\"],\n[-37.9198358, 175.4009195, \"62\"],\n[-37.9214928833, 175.4003147333, \"82\"],\n[-37.9276005833, 175.3979560333, \"156\"],\n[-37.9090980833, 175.47152255, \"112B\"],\n[-37.9100087667, 175.47027605, \"121A\"],\n[-37.90817245, 175.47139345, \"103\"],\n[-37.90884805, 175.4719748, \"108A\"],\n[-37.9089414167, 175.4722395833, \"108B\"],\n[-37.9084711833, 175.47177935, \"104\"],\n[-37.90863345, 175.47171425, \"106\"],\n[-37.9083213167, 175.47131235, \"105\"],\n[-37.9090004, 175.472447, \"108C\"],\n[-37.9028464833, 175.4753390167, \"26B\"],\n[-37.9078277833, 175.4723944833, \"4/96\"],\n[-37.90357775, 175.4740898333, \"36\"],\n[-37.90385065, 175.473554, \"35A\"],\n[-37.9035803667, 175.47369605, \"35\"],\n[-37.90815455, 175.4719365833, \"100\"],\n[-37.91190955, 175.4702316833, \"138\"],\n[-37.90756, 175.4722275333, \"7/96\"],\n[-37.9089549167, 175.47103335, \"111\"],\n[-37.9044076167, 175.4737216, \"42\"],\n[-37.9011103, 175.4747561, \"13\"],\n[-37.9113504333, 175.46916605, \"135B\"],\n[-37.90916025, 175.4717170833, \"112A\"],\n[-37.9104814167, 175.4713397667, \"124B\"],\n[-37.9113419667, 175.4696552833, \"133A\"],\n[-37.9116247167, 175.4707302833, \"134A\"],\n[-37.9118128833, 175.4706304, \"134B\"],\n[-37.9117777333, 175.46919275, \"139A\"],\n[-37.91172915, 175.4689741333, \"139B\"],\n[-37.9091821167, 175.47238525, \"110C\"],\n[-37.9112748167, 175.46989875, \"131A\"],\n[-37.9112112333, 175.46968315, \"131B\"],\n[-37.9018407333, 175.474509, \"17\"],\n[-37.9021059167, 175.4747859167, \"18\"],\n[-37.9022214333, 175.4747533667, \"20\"],\n[-37.9021081667, 175.4743658667, \"21\"],\n[-37.9024045833, 175.4746604333, \"22\"],\n[-37.90227905, 175.4742904, \"23\"],\n[-37.9025434333, 175.4745909167, \"24\"],\n[-37.9033884667, 175.4737883, \"33\"],\n[-37.9029568833, 175.4739659833, \"31\"],\n[-37.90244755, 175.4742011333, \"25\"],\n[-37.9026128, 175.4741282667, \"27\"],\n[-37.9027832333, 175.4740469833, \"29\"],\n[-37.9027372833, 175.4744949833, \"30\"],\n[-37.9028963667, 175.4744151167, \"32\"],\n[-37.9039784167, 175.4735005167, \"39\"],\n[-37.9041466, 175.47341055, \"41\"],\n[-37.9043180167, 175.4733317667, \"43\"],\n[-37.9044306, 175.4732717, \"45\"],\n[-37.9002813333, 175.4746537333, \"3A\"],\n[-37.9003363333, 175.4746345833, \"3B\"],\n[-37.9003208833, 175.4742427, \"3C\"],\n[-37.9004106, 175.47457215, \"3D\"],\n[-37.9008182333, 175.4749564333, \"7\"],\n[-37.9008622667, 175.4745689333, \"9A\"],\n[-37.9087553333, 175.47168745, \"108\"],\n[-37.9087847167, 175.47111335, \"109\"],\n[-37.9090482, 175.4718688667, \"110A\"],\n[-37.9089821833, 175.47162705, \"110\"],\n[-37.9097564, 175.47064845, \"117\"],\n[-37.90991065, 175.4711398333, \"118\"],\n[-37.9098192, 175.4703204, \"119A\"],\n[-37.9100781833, 175.4710667, \"120\"],\n[-37.9102616, 175.4713288833, \"120A\"],\n[-37.91019675, 175.4704249667, \"121\"],\n[-37.9102786333, 175.47095955, \"122\"],\n[-37.91061325, 175.47114525, \"124A\"],\n[-37.9104595333, 175.4709259167, \"124\"],\n[-37.9103273167, 175.4703685833, \"125\"],\n[-37.9009658333, 175.47489005, \"9\"],\n[-37.9106314167, 175.47081795, \"126\"],\n[-37.9105098833, 175.4702934833, \"127\"],\n[-37.9115283, 175.47038715, \"132\"],\n[-37.9114078, 175.4698388833, \"133\"],\n[-37.9117353833, 175.4702887167, \"136\"],\n[-37.91165515, 175.4697282333, \"137\"],\n[-37.91209905, 175.4701214833, \"140\"],\n[-37.91192095, 175.46961505, \"141\"],\n[-37.91209585, 175.4695346667, \"143\"],\n[-37.91223925, 175.4694837833, \"145\"],\n[-37.9129544, 175.46972655, \"148\"],\n[-37.9127850833, 175.46918905, \"149\"],\n[-37.9131046167, 175.4696231333, \"150\"],\n[-37.9129241667, 175.4691591333, \"151\"],\n[-37.9133420333, 175.4695050167, \"152\"],\n[-37.9134355667, 175.4694691333, \"154\"],\n[-37.9136254167, 175.4688124833, \"155\"],\n[-37.9135736667, 175.4693978333, \"156\"],\n[-37.9137485167, 175.4693549667, \"158\"],\n[-37.9144754, 175.46902505, \"164\"],\n[-37.9147488833, 175.4695174667, \"166\"],\n[-37.9147467167, 175.4689613833, \"168\"],\n[-37.9053532, 175.4735344167, \"44A\"],\n[-37.9052717667, 175.4733045167, \"44\"],\n[-37.90506265, 175.4729767833, \"49\"],\n[-37.9052035667, 175.4729156167, \"51\"],\n[-37.9053496, 175.4728617833, \"53\"],\n[-37.9054862667, 175.47279975, \"55\"],\n[-37.9071557, 175.47249445, \"88\"],\n[-37.9011584833, 175.47587265, \"10\"],\n[-37.9012260167, 175.4757750333, \"12\"],\n[-37.9011779333, 175.47526505, \"14\"],\n[-37.9024628, 175.4750374833, \"22A\"],\n[-37.9024647, 175.4751844167, \"22B\"],\n[-37.9004774333, 175.4751149333, \"1\"],\n[-37.9028335, 175.4750389333, \"28\"],\n[-37.9004664333, 175.4755579667, \"2\"],\n[-37.9005692, 175.4755242167, \"4\"],\n[-37.9006539333, 175.4750368333, \"5\"],\n[-37.9010841833, 175.4755968, \"8A\"],\n[-37.9008984167, 175.4753584833, \"8\"],\n[-37.9091240667, 175.4721442833, \"110B\"],\n[-37.9097632833, 175.47034635, \"117A\"],\n[-37.9082253333, 175.4719109833, \"102\"],\n[-37.9114097833, 175.4693441333, \"135A\"],\n[-37.9112057333, 175.4692141667, \"135C\"],\n[-37.91167365, 175.4687884833, \"139D\"],\n[-37.9112668833, 175.4694056333, \"135\"],\n[-37.9111444667, 175.4690280333, \"135D\"],\n[-37.9115951667, 175.4692708167, \"139\"],\n[-37.9115359, 175.4690900833, \"139C\"],\n[-37.9106014, 175.4702607333, \"129\"],\n[-37.9076115833, 175.4723767833, \"98\"],\n[-37.9075872333, 175.4722996, \"8/96\"],\n[-37.9076397833, 175.4724662167, \"10/96\"],\n[-37.9076588167, 175.4725362667, \"11/96\"],\n[-37.9076818333, 175.47261165, \"12/96\"],\n[-37.90787275, 175.4725516833, \"6/96\"],\n[-37.9078548167, 175.4724749, \"5/96\"],\n[-37.9078033833, 175.47231415, \"3/96\"],\n[-37.9077763833, 175.4722320833, \"2/96\"],\n[-37.9077493833, 175.4721500167, \"1/96\"],\n[-37.9027466667, 175.4750991167, \"26A\"],\n[-37.9013304167, 175.4746607667, \"13A\"],\n[-37.9012675, 175.4744082333, \"13B\"],\n[-37.9010857333, 175.4744788167, \"13C\"],\n[-37.91033795, 175.47011845, \"125A\"],\n[-37.90225545, 175.4739976167, \"23A\"],\n[-37.9099696333, 175.4705395667, \"119\"],\n[-37.9152684167, 175.4687266333, \"176\"],\n[-37.8767274167, 175.4498122333, \"31\"],\n[-37.8753801, 175.4141292833, \"347\"],\n[-37.8749301667, 175.4229940833, \"269\"],\n[-37.8780543667, 175.4318462667, \"2/193\"],\n[-37.87844965, 175.4315496667, \"4/193\"],\n[-37.877916, 175.4313673167, \"3/193\"],\n[-37.8772684667, 175.4313525833, \"1/193\"],\n[-37.8745261, 175.41460525, \"336\"],\n[-37.8730149167, 175.4139278833, \"348\"],\n[-37.8749023333, 175.4133807833, \"357\"],\n[-37.8748893, 175.4158492, \"331\"],\n[-37.87500415, 175.4305876333, \"199\"],\n[-37.8762052667, 175.4298853667, \"209\"],\n[-37.876837, 175.4272319333, \"231\"],\n[-37.8750526167, 175.4337470333, \"173\"],\n[-37.8750786167, 175.4326617667, \"185\"],\n[-37.8765961833, 175.4422767333, \"1/101\"],\n[-37.8765928833, 175.44146825, \"2/101\"],\n[-37.8766094667, 175.4450663167, \"1/75\"],\n[-37.8751227667, 175.4479436, \"47\"],\n[-37.8785082667, 175.4463698333, \"59\"],\n[-37.8749291833, 175.42188625, \"275\"],\n[-37.8750447, 175.4367026, \"145\"],\n[-37.8746106333, 175.4284329, \"218\"],\n[-37.8745961333, 175.4294403, \"210\"],\n[-37.8729241167, 175.4301093, \"204\"],\n[-37.8730751833, 175.4315361667, \"192\"],\n[-37.8764695333, 175.4360706167, \"2/151\"],\n[-37.8750668667, 175.4352073833, \"161\"],\n[-37.8745754667, 175.4496806333, \"32\"],\n[-37.87504855, 175.4370912, \"141\"],\n[-37.8745623, 175.4221763333, \"276\"],\n[-37.8745616667, 175.4217338333, \"278\"],\n[-37.8749560667, 175.4206555833, \"291\"],\n[-37.87457185, 175.42039215, \"292\"],\n[-37.8749318, 175.4195155667, \"297\"],\n[-37.8749193167, 175.4189609333, \"301\"],\n[-37.8745523667, 175.4178998833, \"314\"],\n[-37.87555895, 175.4169412167, \"317\"],\n[-37.8727607, 175.4172541833, \"320\"],\n[-37.87489075, 175.41689625, \"325\"],\n[-37.8762137833, 175.4211919167, \"283\"],\n[-37.8773094667, 175.4190741833, \"295\"],\n[-37.8766782167, 175.4177899333, \"307\"],\n[-37.87495795, 175.4292900167, \"211\"],\n[-37.87496815, 175.4277122333, \"229\"],\n[-37.87496315, 175.4270601833, \"235\"],\n[-37.87495715, 175.4262284667, \"243\"],\n[-37.87466625, 175.4340618833, \"166\"],\n[-37.8731281333, 175.43353715, \"176\"],\n[-37.8746368667, 175.43285965, \"180\"],\n[-37.8746353667, 175.4320823333, \"188\"],\n[-37.8746651833, 175.43837135, \"134\"],\n[-37.8746581, 175.4361343333, \"152\"],\n[-37.8766056667, 175.4444213667, \"2/75\"],\n[-37.8750381333, 175.4425046833, \"95\"],\n[-37.8758251333, 175.4478232833, \"49\"],\n[-37.87515175, 175.4469793167, \"51\"],\n[-37.8771227333, 175.4464864167, \"57\"],\n[-37.8750768, 175.445912, \"61\"],\n[-37.8750693833, 175.4452335333, \"71\"],\n[-37.8765724333, 175.4321372667, \"191\"],\n[-37.876819, 175.4318101833, \"1/191\"],\n[-37.8750694, 175.4518701667, \"15\"],\n[-37.8750833, 175.4512497167, \"21\"],\n[-37.87511365, 175.4491755667, \"37\"],\n[-37.8744741833, 175.4113393, \"372\"],\n[-37.87448925, 175.4111035167, \"374\"],\n[-37.8749085333, 175.4183780667, \"309\"],\n[-37.8748987, 175.4119560833, \"371\"],\n[-37.8754877, 175.4485419667, \"43\"],\n[-37.87489105, 175.4142969333, \"1/347\"],\n[-37.87509285, 175.4505922, \"23\"],\n[-37.8746456667, 175.4350032667, \"162\"],\n[-37.8817882333, 175.46555605, \"1\"],\n[-37.8817762333, 175.4657047, \"2\"],\n[-37.88177495, 175.4658300167, \"3\"],\n[-37.8818317167, 175.4659088333, \"4\"],\n[-37.8818769833, 175.4658514167, \"5\"],\n[-37.881892, 175.4657157833, \"6\"],\n[-37.8819030667, 175.4655528667, \"7\"],\n[-37.8249572667, 175.3822644167, \"62C\"],\n[-37.8241013167, 175.3830562833, \"62B\"],\n[-37.8235533667, 175.3836837833, \"62A\"],\n[-37.8232282333, 175.38113915, \"42\"],\n[-37.8216185167, 175.3801976167, \"24\"],\n[-37.8233693667, 175.3818881167, \"50\"],\n[-37.8238237167, 175.38163785, \"48\"],\n[-37.8223544167, 175.3807860167, \"34\"],\n[-37.8221907167, 175.38064385, \"32\"],\n[-37.7917540833, 175.4757575167, \"369\"],\n[-37.7917070167, 175.4768378667, \"371\"],\n[-37.7976220833, 175.4791485667, \"310\"],\n[-37.8026816667, 175.4738649167, \"241\"],\n[-37.8063079833, 175.4719216833, \"198\"],\n[-37.8127222, 175.46321975, \"88\"],\n[-37.79851905, 175.4782582, \"298\"],\n[-37.7982161667, 175.4784413833, \"300\"],\n[-37.7979628667, 175.4785954333, \"304\"],\n[-37.7989472167, 175.4770213333, \"289\"],\n[-37.8124759, 175.46149895, \"66\"],\n[-37.80813105, 175.4692921667, \"133\"],\n[-37.80903515, 175.4695616333, \"164\"],\n[-37.81169535, 175.4617913167, \"77\"],\n[-37.7931904667, 175.4771456167, \"359\"],\n[-37.90154435, 175.4820577833, \"17A\"],\n[-37.9023211333, 175.4866221333, \"50\"],\n[-37.9029791167, 175.4860176833, \"43\"],\n[-37.9018640167, 175.4839159667, \"29\"],\n[-37.9017065667, 175.4845584, \"32\"],\n[-37.9013472667, 175.4832850333, \"20\"],\n[-37.9022597667, 175.4852180667, \"37\"],\n[-37.90235635, 175.4855437333, \"39\"],\n[-37.9020226167, 175.48626825, \"48A\"],\n[-37.90219835, 175.4862075833, \"48\"],\n[-37.9028144833, 175.4859300667, \"41A\"],\n[-37.9019145833, 175.4857036333, \"38\"],\n[-37.9012494167, 175.4836438333, \"22A\"],\n[-37.90117625, 175.4832889167, \"20A\"],\n[-37.9013004333, 175.4831083, \"18A\"],\n[-37.9018345667, 175.48310725, \"23A\"],\n[-37.9014717833, 175.4845211, \"30A\"],\n[-37.9011141833, 175.4813179833, \"11\"],\n[-37.9012595833, 175.4829706167, \"18\"],\n[-37.9012063333, 175.4815285167, \"13\"],\n[-37.90106015, 175.48224675, \"14\"],\n[-37.9014018333, 175.4835019, \"22\"],\n[-37.90164195, 175.4831019333, \"23\"],\n[-37.9014562667, 175.4837066, \"24\"],\n[-37.9017002833, 175.48331585, \"25\"],\n[-37.9013501333, 175.4820586167, \"17\"],\n[-37.9017979167, 175.48370115, \"27A\"],\n[-37.9017437333, 175.4834995, \"27\"],\n[-37.9015959833, 175.4841416, \"28\"],\n[-37.9016565333, 175.4843582167, \"30\"],\n[-37.9019526833, 175.4841925, \"31\"],\n[-37.90202855, 175.4844477667, \"33\"],\n[-37.9008982667, 175.4817133, \"8\"],\n[-37.9024487833, 175.4858792167, \"41\"],\n[-37.9020930667, 175.4858615167, \"42\"],\n[-37.9025991167, 175.4863993167, \"49\"],\n[-37.9021011667, 175.4846921667, \"35\"],\n[-37.9014189333, 175.4823503167, \"19\"],\n[-37.90126485, 175.4817892167, \"15\"],\n[-37.9015360333, 175.4839226833, \"26\"],\n[-37.83531625, 175.4933037333, \"196\"],\n[-37.8352276167, 175.4927147833, \"195\"],\n[-37.8360773, 175.4912368, \"187\"],\n[-37.8346867167, 175.49328105, \"201\"],\n[-37.82529725, 175.5052682333, \"361\"],\n[-37.8370500167, 175.4896684, \"163\"],\n[-37.8396963333, 175.4849568, \"1/133\"],\n[-37.8292152667, 175.4972928333, \"281\"],\n[-37.8293017833, 175.4982716333, \"300\"],\n[-37.8282765333, 175.4989055, \"307\"],\n[-37.8274501333, 175.5007519833, \"333\"],\n[-37.8273441167, 175.5024751333, \"2/336\"],\n[-37.8261453167, 175.5047067667, \"360\"],\n[-37.8301054, 175.4958695333, \"275\"],\n[-37.8271283667, 175.5028555, \"1/336\"],\n[-37.8369560667, 175.4905711833, \"172\"],\n[-37.81904615, 175.5025335167, \"7/369\"],\n[-37.8218408167, 175.50377335, \"4/369\"],\n[-37.8209456333, 175.5022760833, \"6/369\"],\n[-37.8208349667, 175.5044007, \"5/369\"],\n[-37.8238658, 175.50523275, \"2/369\"],\n[-37.8190694, 175.5003232333, \"8/369\"],\n[-37.8120237667, 175.5010552833, \"10/369\"],\n[-37.8118820833, 175.50392555, \"11/369\"],\n[-37.8356394, 175.4927849, \"192\"],\n[-37.8452920167, 175.4770441167, \"6\"],\n[-37.8332265167, 175.4939223333, \"232\"],\n[-37.8297708, 175.4960635, \"253\"],\n[-37.8393579167, 175.4857046667, \"2/133\"],\n[-37.83878395, 175.48766725, \"142\"],\n[-37.8379286333, 175.4890502, \"152\"],\n[-37.8364941, 175.4914751, \"186\"],\n[-37.8248432833, 175.5066992833, \"369\"],\n[-37.82553745, 175.5068898833, \"380\"],\n[-37.83202985, 175.4941239333, \"240\"],\n[-37.82194035, 175.5057576833, \"3/369\"],\n[-37.8227608167, 175.5063209333, \"1/369\"],\n[-37.8141088167, 175.5037792333, \"9/369\"],\n[-37.8382942833, 175.48852635, \"148\"],\n[-37.8603913833, 175.4492696, \"57\"],\n[-37.8592938167, 175.42099095, \"308C\"],\n[-37.85984295, 175.42067725, \"308B\"],\n[-37.8598229167, 175.4210151667, \"308A\"],\n[-37.8598344333, 175.3976947833, \"508\"],\n[-37.8603243333, 175.4140272833, \"369\"],\n[-37.8603184, 175.4123881167, \"385\"],\n[-37.86106375, 175.4477743833, \"67\"],\n[-37.861365, 175.4476857667, \"73\"],\n[-37.8613431833, 175.4479565167, \"69\"],\n[-37.86191185, 175.44788405, \"71\"],\n[-37.8603987333, 175.4502737667, \"53\"],\n[-37.86045645, 175.4319935167, \"209\"],\n[-37.8603551333, 175.4258720333, \"263\"],\n[-37.8599289833, 175.4315900833, \"210\"],\n[-37.8604198833, 175.4334522667, \"195\"],\n[-37.8627496167, 175.42078385, \"309B\"],\n[-37.8609723167, 175.4515878667, \"37\"],\n[-37.8603717833, 175.4514413333, \"39\"],\n[-37.8609809, 175.4517044667, \"35\"],\n[-37.86096795, 175.45250335, \"27\"],\n[-37.8604741333, 175.4530975833, \"17\"],\n[-37.8603852667, 175.451807, \"33\"],\n[-37.8572038, 175.42770555, \"246\"],\n[-37.8604631667, 175.4363373667, \"171\"],\n[-37.8591713833, 175.4484903833, \"62\"],\n[-37.8604119667, 175.4340429333, \"191\"],\n[-37.8603663667, 175.4354625167, \"179\"],\n[-37.8603935, 175.4373993667, \"161\"],\n[-37.8600009, 175.44318935, \"108\"],\n[-37.8599224167, 175.4265426167, \"256\"],\n[-37.8612047333, 175.4464131333, \"81B\"],\n[-37.8599028667, 175.4231039167, \"288\"],\n[-37.8602706, 175.4220058333, \"295\"],\n[-37.8602638667, 175.4191497333, \"323\"],\n[-37.8609657667, 175.4187969, \"327\"],\n[-37.8602931, 175.4179491333, \"329\"],\n[-37.8598848667, 175.4176532167, \"338\"],\n[-37.8632475167, 175.4173751333, \"339\"],\n[-37.8599251667, 175.4255647333, \"1/264\"],\n[-37.8599279667, 175.4253799333, \"2/264\"],\n[-37.86045495, 175.4383567333, \"157\"],\n[-37.8604267333, 175.4421660333, \"113\"],\n[-37.8617114333, 175.4414719167, \"125\"],\n[-37.8603943167, 175.43982245, \"143\"],\n[-37.8604978, 175.4449393667, \"89\"],\n[-37.8603957167, 175.4488193667, \"59\"],\n[-37.8599959167, 175.4481568167, \"64\"],\n[-37.8603463333, 175.4481201667, \"65\"],\n[-37.8603207833, 175.44708855, \"75\"],\n[-37.8617075, 175.4464497667, \"85\"],\n[-37.8603354667, 175.4458761167, \"87\"],\n[-37.8603924833, 175.4529279333, \"21\"],\n[-37.8603805, 175.4522459167, \"31\"],\n[-37.8603894333, 175.4465241, \"81\"],\n[-37.8624513667, 175.4464558667, \"2/85\"],\n[-37.8603426, 175.42038605, \"309A\"],\n[-37.8597382167, 175.4030150833, \"468\"],\n[-37.8600008333, 175.4468853333, \"78\"],\n[-37.8600151833, 175.4476529333, \"70\"],\n[-37.8619444167, 175.4473194667, \"77\"],\n[-37.8604090333, 175.4206673167, \"309\"],\n[-37.8599482667, 175.4450833833, \"90\"],\n[-37.8599667333, 175.4439503167, \"102\"],\n[-37.8599639167, 175.4443829333, \"98\"],\n[-37.8604016667, 175.4435244167, \"107\"],\n[-37.8602872667, 175.4157777167, \"357C\"],\n[-37.8619551667, 175.4154903667, \"357B\"],\n[-37.8602759333, 175.41535695, \"357\"],\n[-37.8599460833, 175.4464074167, \"82\"],\n[-37.8585100167, 175.4461918667, \"84\"],\n[-37.8604238667, 175.4437588667, \"105\"],\n[-37.86030515, 175.4229068167, \"289\"],\n[-37.9165466667, 175.5400699667, \"47\"],\n[-37.9163835, 175.5395208667, \"52\"],\n[-37.9162024833, 175.5415235333, \"4/35\"],\n[-37.9138618, 175.5389959167, \"20\"],\n[-37.9147363, 175.5367299167, \"4\"],\n[-37.9171924833, 175.5398619667, \"53\"],\n[-37.9156216333, 175.5400467167, \"1/35\"],\n[-37.9156090167, 175.5406467667, \"2/35\"],\n[-37.9155483, 175.54130355, \"3/35\"],\n[-37.9161911333, 175.5412111333, \"5/35\"],\n[-37.9177019333, 175.5406090667, \"57\"],\n[-37.9172073167, 175.5390771667, \"56\"],\n[-37.8413934333, 175.3669168333, \"185\"],\n[-37.8417850333, 175.3689200833, \"199\"],\n[-37.8428339667, 175.3706989, \"216\"],\n[-37.8416074, 175.36479145, \"164\"],\n[-37.8409904167, 175.3645986833, \"157\"],\n[-37.84092815, 175.3644066167, \"161\"],\n[-37.8749487833, 175.4757354167, \"4\"],\n[-37.8748824667, 175.4752927333, \"3\"],\n[-37.8746354, 175.475246, \"5\"],\n[-37.8742075167, 175.4755986333, \"10\"],\n[-37.8743622667, 175.4759894667, \"8A\"],\n[-37.8744567667, 175.4756695333, \"8\"],\n[-37.8748683833, 175.4760352167, \"4A\"],\n[-37.8742254333, 175.47601205, \"10A\"],\n[-37.8747158167, 175.4760297167, \"6A\"],\n[-37.8746737833, 175.4757186333, \"6\"],\n[-37.9465236167, 175.3809737667, \"672\"],\n[-37.9005455, 175.4730509333, \"22\"],\n[-37.9003672167, 175.4684980833, \"17\"],\n[-37.9003614667, 175.4682696667, \"13\"],\n[-37.9009926333, 175.4714326, \"18B\"],\n[-37.9004931, 175.4733237333, \"24\"],\n[-37.90023555, 175.4663222333, \"3\"],\n[-37.9003731333, 175.4738848333, \"30\"],\n[-37.90041915, 175.4736641333, \"28A\"],\n[-37.9008516833, 175.47143275, \"18A\"],\n[-37.9009989333, 175.4716885333, \"18C\"],\n[-37.9011166833, 175.4715942833, \"18D\"],\n[-37.9013468667, 175.4716314833, \"18E\"],\n[-37.9013142333, 175.4713999333, \"18F\"],\n[-37.9007320667, 175.4678888833, \"12\"],\n[-37.9007490333, 175.4682728833, \"14\"],\n[-37.9002906, 175.4672436333, \"11\"],\n[-37.9005921667, 175.4728228833, \"20\"],\n[-37.9012265333, 175.4713549, \"18G\"],\n[-37.8998487333, 175.4663062167, \"1\"],\n[-37.9004667833, 175.4734721, \"26\"],\n[-37.9007287833, 175.4668388, \"4\"],\n[-37.9002529667, 175.4665572, \"5\"],\n[-37.9002680333, 175.4668009667, \"7\"],\n[-37.9002738833, 175.4670198667, \"9\"],\n[-37.90052665, 175.4737441167, \"28B\"],\n[-37.9202440833, 175.4647199333, \"59\"],\n[-37.9186491, 175.4631960333, \"74\"],\n[-37.9197376, 175.46404755, \"63\"],\n[-37.9209585333, 175.4715133667, \"31\"],\n[-37.92198325, 175.4719067667, \"32\"],\n[-37.9200168333, 175.4637374167, \"64\"],\n[-37.9199907667, 175.4714465333, \"9\"],\n[-37.91941015, 175.4649217833, \"65\"],\n[-37.9196411667, 175.4633694333, \"66\"],\n[-37.91916365, 175.4639929833, \"67A\"],\n[-37.9194144667, 175.4637018833, \"67\"],\n[-37.9193556833, 175.4631293833, \"70\"],\n[-37.9191286667, 175.4630976, \"72\"],\n[-37.9189485833, 175.4635427667, \"69\"],\n[-37.9201641167, 175.4662894167, \"55\"],\n[-37.9211940333, 175.4656174833, \"56A\"],\n[-37.920671, 175.4657966667, \"56\"],\n[-37.92006415, 175.4658042, \"57A\"],\n[-37.9213706, 175.4727426833, \"23\"],\n[-37.9214441667, 175.4732278667, \"24\"],\n[-37.9202821667, 175.4732763833, \"18\"],\n[-37.9204884667, 175.4729263, \"19\"],\n[-37.9211967, 175.47098555, \"33\"],\n[-37.92094835, 175.4706909167, \"35\"],\n[-37.9219078167, 175.47147535, \"34\"],\n[-37.9197843667, 175.46309625, \"68\"],\n[-37.9210935333, 175.46484215, \"60A\"],\n[-37.9212236167, 175.4704147167, \"40\"],\n[-37.9197908, 175.4659149333, \"55A\"],\n[-37.92033405, 175.4655102, \"57\"],\n[-37.9211895167, 175.46547285, \"58A\"],\n[-37.9207404, 175.46524515, \"58\"],\n[-37.91857275, 175.4635986167, \"71\"],\n[-37.9182674667, 175.46367495, \"73\"],\n[-37.9195246333, 175.4715611, \"11\"],\n[-37.9216761833, 175.4729861167, \"26\"],\n[-37.92162375, 175.4721619667, \"27\"],\n[-37.9218544, 175.4726169833, \"28\"],\n[-37.9215389333, 175.4715153167, \"29\"],\n[-37.9219467167, 175.4722808333, \"30\"],\n[-37.9193754, 175.4726740833, \"12\"],\n[-37.9195867667, 175.4721921667, \"13\"],\n[-37.9196743333, 175.4735581167, \"14A\"],\n[-37.91978385, 175.4729767167, \"14\"],\n[-37.9200549833, 175.4726106667, \"15\"],\n[-37.9197512333, 175.4737709, \"16\"],\n[-37.9204467, 175.4722458667, \"17\"],\n[-37.9199559, 175.4736553667, \"18A\"],\n[-37.9193368833, 175.4700432833, \"1\"],\n[-37.9218013667, 175.4711677833, \"36\"],\n[-37.9204076, 175.4708148667, \"37\"],\n[-37.9217517833, 175.4702188667, \"38A\"],\n[-37.9215826, 175.4708205333, \"38\"],\n[-37.9207000333, 175.4703961167, \"39\"],\n[-37.9205687167, 175.4700339833, \"41\"],\n[-37.9208976, 175.46987095, \"42\"],\n[-37.92033355, 175.4687946, \"45\"],\n[-37.91982325, 175.4679912667, \"47A\"],\n[-37.9209613833, 175.4677278167, \"48A\"],\n[-37.92131875, 175.4676346167, \"48B\"],\n[-37.9205986, 175.4680208, \"48\"],\n[-37.9196399667, 175.4679596667, \"49B\"],\n[-37.9202544833, 175.4681937167, \"47\"],\n[-37.9198937833, 175.4678851833, \"49A\"],\n[-37.9205374667, 175.46738275, \"50\"],\n[-37.9213241, 175.4675107, \"50B\"],\n[-37.9201188667, 175.4669429, \"51\"],\n[-37.92103205, 175.4665559333, \"52A\"],\n[-37.9205449167, 175.4668373833, \"52\"],\n[-37.9198923833, 175.46667, \"53\"],\n[-37.9210808167, 175.4664227833, \"54A\"],\n[-37.920561, 175.4662841333, \"54\"],\n[-37.91946705, 175.4707291667, \"5\"],\n[-37.9193920833, 175.47035955, \"3\"],\n[-37.9195288167, 175.4710633833, \"7\"],\n[-37.92069515, 175.4648828167, \"60\"],\n[-37.9209773667, 175.4675400333, \"50A\"],\n[-37.9201842667, 175.46752845, \"49\"],\n[-37.9200461167, 175.4643875333, \"61\"],\n[-37.9205402833, 175.4644506333, \"62\"],\n[-37.9196651333, 175.4646621833, \"63A\"],\n[-37.9211008833, 175.4733943, \"22\"],\n[-37.9207025167, 175.4734697333, \"20\"],\n[-37.9209406667, 175.47297795, \"21\"],\n[-37.9211156333, 175.4720907167, \"25\"],\n[-37.9221545333, 175.4731278833, \"26A\"],\n[-37.8946718167, 175.4691448667, \"53\"],\n[-37.8945620667, 175.46918055, \"55\"],\n[-37.8942623667, 175.4697152833, \"52\"],\n[-37.894203, 175.4688735167, \"65\"],\n[-37.89425415, 175.4689361333, \"63\"],\n[-37.89414905, 175.4688349167, \"67\"],\n[-37.8939181333, 175.4694227167, \"64\"],\n[-37.8938391333, 175.4693460667, \"66\"],\n[-37.8907021167, 175.4667148333, \"93\"],\n[-37.8906039, 175.4666683167, \"95\"],\n[-37.8902027167, 175.4664942833, \"103\"],\n[-37.8905652, 175.4673196167, \"92\"],\n[-37.8898694, 175.4663493333, \"107\"],\n[-37.8897077333, 175.4662881167, \"113\"],\n[-37.8897784833, 175.4663228, \"111\"],\n[-37.8956830333, 175.4710189333, \"32\"],\n[-37.8955352667, 175.4709092833, \"36\"],\n[-37.8954734333, 175.4708266833, \"38\"],\n[-37.8960418333, 175.47138995, \"28\"],\n[-37.8820345667, 175.46329545, \"3/201\"],\n[-37.8822174333, 175.46335335, \"1/201\"],\n[-37.8821085167, 175.4633248, \"2/201\"],\n[-37.8823766667, 175.4634171667, \"197\"],\n[-37.8841371, 175.46403415, \"175A\"],\n[-37.8841525333, 175.4636656, \"175B\"],\n[-37.8864762667, 175.4645354333, \"151\"],\n[-37.8863782833, 175.4644388833, \"153\"],\n[-37.8862153, 175.4648985833, \"155\"],\n[-37.8860901, 175.4648409833, \"157\"],\n[-37.8880628167, 175.4656834833, \"133\"],\n[-37.8878619333, 175.4655976667, \"135\"],\n[-37.8906979667, 175.4673655, \"90\"],\n[-37.8907991, 175.4667608667, \"91\"],\n[-37.8961849167, 175.4707743833, \"25\"],\n[-37.8836823667, 175.4638652167, \"181\"],\n[-37.8836385667, 175.4632253833, \"183\"],\n[-37.8838402, 175.4639201167, \"179\"],\n[-37.8982437333, 175.4734017333, \"1\"],\n[-37.8849776167, 175.46438955, \"171\"],\n[-37.8843719833, 175.4641355167, \"173\"],\n[-37.8839963167, 175.4632512833, \"177\"],\n[-37.88393085, 175.4634884, \"179B\"],\n[-37.8837384333, 175.4634720333, \"181A\"],\n[-37.8834786833, 175.4638174667, \"185\"],\n[-37.8834804667, 175.46322915, \"187\"],\n[-37.8833056667, 175.4637555833, \"189\"],\n[-37.88310485, 175.4633649667, \"191A\"],\n[-37.8831340833, 175.4636871, \"191\"],\n[-37.8830253167, 175.4636718333, \"193\"],\n[-37.8819504, 175.4632695667, \"203\"],\n[-37.88170725, 175.4631031, \"205\"],\n[-37.88149825, 175.4630156833, \"207\"],\n[-37.8813178333, 175.4626260667, \"209A\"],\n[-37.8813002167, 175.4629425167, \"209\"],\n[-37.8810862833, 175.4628550167, \"211\"],\n[-37.8884475, 175.46494685, \"125A\"],\n[-37.88836565, 175.4649932, \"127A\"],\n[-37.88680255, 175.4645032, \"145\"],\n[-37.8866910333, 175.4647031667, \"147A\"],\n[-37.8863981167, 175.4649715167, \"149\"],\n[-37.8859491, 175.4647828333, \"159\"],\n[-37.8857348333, 175.4647021333, \"161\"],\n[-37.8856289833, 175.4642462833, \"163\"],\n[-37.8854787333, 175.4646066833, \"165\"],\n[-37.8853169667, 175.4645344833, \"167\"],\n[-37.8851657167, 175.4644626667, \"169\"],\n[-37.89008265, 175.4664379167, \"105\"],\n[-37.8888476167, 175.4659953167, \"117\"],\n[-37.88862475, 175.4659086167, \"121\"],\n[-37.8884613833, 175.4658299, \"123\"],\n[-37.8884445167, 175.4652906, \"125\"],\n[-37.8883496833, 175.46527395, \"127\"],\n[-37.8882554833, 175.46575415, \"129\"],\n[-37.8881798833, 175.4652017833, \"131\"],\n[-37.8876298667, 175.4655107667, \"137\"],\n[-37.8874722667, 175.4654312667, \"139\"],\n[-37.8870441833, 175.4652550333, \"141\"],\n[-37.8868197, 175.46516795, \"143\"],\n[-37.8865865, 175.46503135, \"147\"],\n[-37.8944378167, 175.4698605333, \"48\"],\n[-37.8943299667, 175.4697972167, \"50\"],\n[-37.8946586167, 175.46927655, \"51\"],\n[-37.8942089833, 175.4696627, \"54\"],\n[-37.89410215, 175.46963495, \"56\"],\n[-37.8944861833, 175.4691251333, \"57\"],\n[-37.89410185, 175.4695671167, \"58\"],\n[-37.89440395, 175.4690316833, \"59\"],\n[-37.8940363833, 175.4695254833, \"60\"],\n[-37.8943071333, 175.4689793333, \"61\"],\n[-37.8939694333, 175.4694826, \"62\"],\n[-37.8937483667, 175.4692441333, \"68\"],\n[-37.8940153, 175.4687127667, \"69\"],\n[-37.8936944333, 175.4691774333, \"70\"],\n[-37.8939444833, 175.4686454167, \"71\"],\n[-37.8914311167, 175.4676565833, \"78\"],\n[-37.8914605833, 175.4668025833, \"81\"],\n[-37.8911125833, 175.467531, \"82\"],\n[-37.8913374667, 175.46691925, \"83\"],\n[-37.8909541667, 175.4674899167, \"84\"],\n[-37.8912424167, 175.4668760167, \"85\"],\n[-37.89086865, 175.4674540833, \"86\"],\n[-37.8911290333, 175.46682535, \"87\"],\n[-37.8908065333, 175.4674188167, \"88\"],\n[-37.8909961333, 175.4667623333, \"89\"],\n[-37.89043345, 175.4672657667, \"98\"],\n[-37.8976105333, 175.47313585, \"10\"],\n[-37.8975240667, 175.47213955, \"17\"],\n[-37.8964341, 175.4719897, \"18\"],\n[-37.8965768167, 175.4718193667, \"20\"],\n[-37.8964536167, 175.47098525, \"21\"],\n[-37.8965168, 175.4717821667, \"22\"],\n[-37.8963066833, 175.4708868167, \"23\"],\n[-37.8964117, 175.4716877833, \"24\"],\n[-37.8973729833, 175.4718744167, \"19\"],\n[-37.8961056167, 175.4707024167, \"27\"],\n[-37.89606105, 175.4706588333, \"29\"],\n[-37.8957499, 175.4710900333, \"30\"],\n[-37.8960081167, 175.4706084667, \"31\"],\n[-37.89595535, 175.4705515167, \"33\"],\n[-37.8962650667, 175.4715582333, \"26\"],\n[-37.8977395833, 175.4736163833, \"2A\"],\n[-37.8976182167, 175.4734736333, \"2\"],\n[-37.8950879167, 175.4697355, \"49\"],\n[-37.8953451167, 175.47080125, \"40\"],\n[-37.8952480833, 175.4706864833, \"42\"],\n[-37.8953311333, 175.4699905167, \"43\"],\n[-37.8951690167, 175.4705682833, \"44\"],\n[-37.8952598667, 175.4699087, \"45\"],\n[-37.8951447333, 175.4705209333, \"46\"],\n[-37.8951409333, 175.4698058667, \"47\"],\n[-37.8954689333, 175.4701014167, \"41A\"],\n[-37.8958601667, 175.4704608833, \"35\"],\n[-37.8957632833, 175.4703618, \"37\"],\n[-37.89565655, 175.4702728333, \"39\"],\n[-37.8955528, 175.4701858833, \"41\"],\n[-37.8976127167, 175.4736873167, \"4A\"],\n[-37.8974748, 175.4735857667, \"4\"],\n[-37.8973855833, 175.4735356667, \"6\"],\n[-37.8974473167, 175.4733615667, \"8\"],\n[-37.8896038, 175.4662530833, \"115\"],\n[-37.8938700833, 175.46857325, \"73\"],\n[-37.8913213333, 175.4676180667, \"80\"],\n[-37.89351535, 175.4690702167, \"76\"],\n[-37.8936501, 175.4683802833, \"77\"],\n[-37.8937599333, 175.4684853667, \"75\"],\n[-37.8935794833, 175.4691255, \"72\"],\n[-37.88384375, 175.4636547333, \"179A\"],\n[-37.8877088, 175.4661684833, \"104\"],\n[-37.8853223833, 175.4655525667, \"128B\"],\n[-37.88438085, 175.4648718667, \"136C\"],\n[-37.8841924667, 175.46482515, \"138\"],\n[-37.8840040333, 175.4647229667, \"140\"],\n[-37.8838259833, 175.4646471667, \"142\"],\n[-37.8836846833, 175.4645814167, \"144\"],\n[-37.8833978167, 175.4647726167, \"146A\"],\n[-37.88347465, 175.46450515, \"146\"],\n[-37.8832495667, 175.4646684333, \"148B\"],\n[-37.8833107333, 175.4644183167, \"148A\"],\n[-37.8830987333, 175.4643600833, \"150\"],\n[-37.88243095, 175.4641342, \"152\"],\n[-37.8822347333, 175.4643721667, \"154A\"],\n[-37.88231865, 175.4640810833, \"154\"],\n[-37.8820192, 175.4644994833, \"156\"],\n[-37.8820541833, 175.46428995, \"158A\"],\n[-37.8821268, 175.4640207, \"158\"],\n[-37.88190465, 175.4639371333, \"160\"],\n[-37.8817260167, 175.4638787833, \"162\"],\n[-37.8814407667, 175.4640942167, \"164A\"],\n[-37.8815740333, 175.46375875, \"164\"],\n[-37.8814061667, 175.4637101333, \"166\"],\n[-37.8812219167, 175.4636397667, \"168\"],\n[-37.8810714, 175.4635707167, \"170\"],\n[-37.8849612333, 175.4651687333, \"134\"],\n[-37.8867657833, 175.46587165, \"116\"],\n[-37.8873525667, 175.4663687, \"108A\"],\n[-37.8875141333, 175.46643145, \"108B\"],\n[-37.88740205, 175.4660429667, \"108\"],\n[-37.8872605, 175.46600665, \"110\"],\n[-37.8870516667, 175.4659278333, \"114\"],\n[-37.8859261833, 175.4657479333, \"122A\"],\n[-37.8859050667, 175.4659105167, \"122B\"],\n[-37.8859588, 175.4655614, \"122\"],\n[-37.88575615, 175.4657199833, \"124A\"],\n[-37.8857864167, 175.4655019333, \"124\"],\n[-37.8855956333, 175.46541875, \"126\"],\n[-37.8854599, 175.4652737833, \"128\"],\n[-37.8852678333, 175.46529905, \"130\"],\n[-37.8850946667, 175.4652269667, \"132\"],\n[-37.8878558833, 175.4662192833, \"100\"],\n[-37.88434005, 175.4648704833, \"136B\"],\n[-37.88430585, 175.4648585667, \"136A\"],\n[-37.9102085667, 175.47267125, \"50B\"],\n[-37.9116658, 175.4774299167, \"86A\"],\n[-37.91171815, 175.4771741167, \"82A\"],\n[-37.90942125, 175.47189105, \"37\"],\n[-37.91030625, 175.47526205, \"63B\"],\n[-37.9104127167, 175.4752166, \"63A\"],\n[-37.9095648833, 175.47290545, \"49A\"],\n[-37.9093032333, 175.4714794833, \"35A\"],\n[-37.90934235, 175.47164185, \"35B\"],\n[-37.9093857333, 175.4728978167, \"47B\"],\n[-37.9095356333, 175.4723232833, \"43\"],\n[-37.9094703333, 175.4721117667, \"39\"],\n[-37.9096594333, 175.4715342167, \"40\"],\n[-37.9080098833, 175.4672010667, \"5\"],\n[-37.9083293833, 175.4682696833, \"13\"],\n[-37.9089423167, 175.4690827667, \"20\"],\n[-37.9089409167, 175.4702961167, \"29\"],\n[-37.9081540667, 175.46658445, \"2\"],\n[-37.9097415167, 175.4717528833, \"42\"],\n[-37.9101694333, 175.4746051167, \"57\"],\n[-37.9080884, 175.4674672833, \"7\"],\n[-37.91215875, 175.4797119167, \"102\"],\n[-37.9118344667, 175.4799542, \"103\"],\n[-37.9106944, 175.4744461, \"62A\"],\n[-37.9113962667, 175.4772112333, \"82\"],\n[-37.90982995, 175.4733563, \"51A\"],\n[-37.9115914833, 175.4801362333, \"107A\"],\n[-37.9092558833, 175.4696357, \"26B\"],\n[-37.91002055, 175.4726635167, \"50A\"],\n[-37.90847515, 175.46755925, \"10\"],\n[-37.9082532667, 175.4680019333, \"11\"],\n[-37.9088822833, 175.4688571833, \"18\"],\n[-37.9086208167, 175.4692152333, \"19\"],\n[-37.9085374333, 175.4677686333, \"12\"],\n[-37.9086063833, 175.4679739333, \"14\"],\n[-37.9086596833, 175.4681819, \"16\"],\n[-37.9086889667, 175.4694297167, \"21\"],\n[-37.9089948667, 175.4692835833, \"22\"],\n[-37.9087513667, 175.4696475333, \"23\"],\n[-37.9090557667, 175.4694965167, \"24\"],\n[-37.9088116333, 175.4698680667, \"25\"],\n[-37.9091091, 175.46969345, \"26\"],\n[-37.907833, 175.4666559167, \"1\"],\n[-37.9091789833, 175.4699557667, \"28\"],\n[-37.9096087, 175.4713450667, \"38\"],\n[-37.9092433833, 175.4701392333, \"30\"],\n[-37.9090019833, 175.47051235, \"31\"],\n[-37.9093068333, 175.4703555833, \"32\"],\n[-37.9090940833, 175.4707707667, \"33\"],\n[-37.90938145, 175.4705582833, \"34\"],\n[-37.9094346667, 175.4707651333, \"36\"],\n[-37.9088761167, 175.47007805, \"27\"],\n[-37.9098069333, 175.4719604833, \"44\"],\n[-37.9098796833, 175.4721707667, \"46\"],\n[-37.909607, 175.4725725667, \"45\"],\n[-37.90920615, 175.4729688333, \"47A\"],\n[-37.9093402667, 175.4728171, \"47\"],\n[-37.9099493833, 175.4724105167, \"48\"],\n[-37.9096936333, 175.47284265, \"49\"],\n[-37.9079234833, 175.4669311833, \"3\"],\n[-37.9082463667, 175.4668155333, \"4\"],\n[-37.9097714167, 175.4731308333, \"51\"],\n[-37.9100506833, 175.4729631167, \"52\"],\n[-37.9101362833, 175.473234, \"54\"],\n[-37.9105130333, 175.4743347667, \"60\"],\n[-37.91028125, 175.4747888167, \"59\"],\n[-37.9105830167, 175.4745966167, \"62\"],\n[-37.9106531, 175.4748493333, \"64\"],\n[-37.90835855, 175.4671917333, \"8\"],\n[-37.9081653833, 175.4677261, \"9\"],\n[-37.9120752833, 175.4794866167, \"100\"],\n[-37.9117925167, 175.47970425, \"101\"],\n[-37.91190545, 175.4801769333, \"105\"],\n[-37.9122550333, 175.4799224833, \"106\"],\n[-37.9116940333, 175.48024145, \"107\"],\n[-37.9124476, 175.4801052833, \"108\"],\n[-37.9117246833, 175.4803671833, \"109\"],\n[-37.9103405167, 175.4750005667, \"61\"],\n[-37.9104726833, 175.4754203667, \"65\"],\n[-37.9107200333, 175.47506155, \"66\"],\n[-37.91053305, 175.4756331, \"67\"],\n[-37.9111229833, 175.476363, \"74\"],\n[-37.9114657833, 175.4764926, \"76A\"],\n[-37.911183, 175.4765541167, \"76\"],\n[-37.91150505, 175.47660575, \"78A\"],\n[-37.9112499833, 175.4767965667, \"78\"],\n[-37.9115982333, 175.4769201167, \"80A\"],\n[-37.9113175167, 175.4769843333, \"80\"],\n[-37.91152935, 175.4777795, \"88\"],\n[-37.9117877, 175.4784990667, \"92\"],\n[-37.9118644667, 175.4787878833, \"94\"],\n[-37.9121825833, 175.4787955833, \"96A\"],\n[-37.9119467333, 175.47904265, \"96\"],\n[-37.9119913167, 175.4792895833, \"98\"],\n[-37.9117365667, 175.47948695, \"99\"],\n[-37.9107912667, 175.4752822833, \"68\"],\n[-37.9108477833, 175.4754937167, \"70\"],\n[-37.9118233, 175.4773533, \"86B\"],\n[-37.9119954167, 175.47727035, \"86C\"],\n[-37.9115303833, 175.4774913167, \"86\"],\n[-37.9082149, 175.4684349833, \"13A\"],\n[-37.8870698333, 175.4663411167, \"1-13\"],\n[-37.8871554667, 175.4663601, \"2-12\"],\n[-37.9016082667, 175.4806621, \"30\"],\n[-37.9010984667, 175.4803028833, \"22\"],\n[-37.9012443833, 175.4808339333, \"24\"],\n[-37.9013056667, 175.4802206, \"26\"],\n[-37.90149895, 175.4805675833, \"28\"],\n[-37.9015629833, 175.4800562667, \"32\"],\n[-37.90147835, 175.4797437167, \"45\"],\n[-37.8996289167, 175.4806158667, \"2A\"],\n[-37.89956085, 175.4797871333, \"11\"],\n[-37.9009366167, 175.4793407667, \"37\"],\n[-37.9010581667, 175.4795131167, \"39\"],\n[-37.8992862833, 175.4802488833, \"5\"],\n[-37.90020145, 175.4797472, \"10\"],\n[-37.9003469333, 175.47964355, \"12\"],\n[-37.89972725, 175.47967295, \"13\"],\n[-37.9006396, 175.4794276, \"14\"],\n[-37.8998769167, 175.47947625, \"15\"],\n[-37.9007610667, 175.4795863833, \"16\"],\n[-37.9008528833, 175.47974655, \"18\"],\n[-37.9009693167, 175.4799194, \"20\"],\n[-37.90002065, 175.47947515, \"17\"],\n[-37.9001685333, 175.4793572, \"19\"],\n[-37.8993512667, 175.4806693, \"1\"],\n[-37.9003158833, 175.479241, \"21\"],\n[-37.9004667167, 175.4791244167, \"23\"],\n[-37.9006037, 175.4790302167, \"25\"],\n[-37.90070445, 175.4786061667, \"27\"],\n[-37.9007410667, 175.4788370167, \"29\"],\n[-37.9008196333, 175.4791513333, \"31\"],\n[-37.901124, 175.4788828667, \"33\"],\n[-37.90119195, 175.4790490333, \"35\"],\n[-37.9013381833, 175.4794026, \"41\"],\n[-37.8997734, 175.4805030333, \"2\"],\n[-37.8992978333, 175.4804472333, \"3\"],\n[-37.9012097667, 175.4797523667, \"43\"],\n[-37.8996789, 175.48033335, \"4\"],\n[-37.8998323667, 175.4800290167, \"6\"],\n[-37.8993387167, 175.4800615833, \"7\"],\n[-37.9000414667, 175.4798654, \"8\"],\n[-37.8994266333, 175.47991435, \"9\"],\n[-37.8172086167, 175.36975015, \"17\"],\n[-37.8189982167, 175.3714596333, \"28C\"],\n[-37.8174889333, 175.3716284333, \"35\"],\n[-37.8180339667, 175.3717238667, \"36\"],\n[-37.8172767, 175.3702897333, \"25\"],\n[-37.8183033167, 175.3704190333, \"28A\"],\n[-37.8177806167, 175.3709889833, \"30\"],\n[-37.8184724667, 175.3714858167, \"34\"],\n[-37.8172573833, 175.3723560333, \"37\"],\n[-37.8175974333, 175.3697317, \"18\"],\n[-37.8189201833, 175.3708153, \"28B\"],\n[-37.8170975, 175.3688759833, \"7\"],\n[-37.8174008333, 175.3711639167, \"31\"],\n[-37.8177271333, 175.3719905167, \"39\"],\n[-37.9105104667, 175.4696305667, \"29\"],\n[-37.9105738333, 175.4698784833, \"33\"],\n[-37.9119496333, 175.4744651, \"75\"],\n[-37.9124263667, 175.4747625167, \"76\"],\n[-37.9120037833, 175.47464375, \"77A\"],\n[-37.9103653167, 175.4676619667, \"20\"],\n[-37.9108393333, 175.4707173, \"41\"],\n[-37.91176765, 175.4719875333, \"58A\"],\n[-37.9116183, 175.4720772, \"58\"],\n[-37.9128712, 175.4761248167, \"86\"],\n[-37.9124839333, 175.47629265, \"85\"],\n[-37.9104650167, 175.4694600667, \"27\"],\n[-37.9109299, 175.4697178333, \"40\"],\n[-37.9109971667, 175.4699570333, \"42\"],\n[-37.9097695, 175.4672281333, \"11\"],\n[-37.9100539667, 175.4665771, \"10\"],\n[-37.91041835, 175.4666636, \"12A\"],\n[-37.9101176333, 175.4667979833, \"12\"],\n[-37.90983315, 175.4674340333, \"13\"],\n[-37.9104875, 175.4669176667, \"14A\"],\n[-37.9101736833, 175.4670051, \"14\"],\n[-37.9098833833, 175.46762845, \"15\"],\n[-37.9102367667, 175.4672218833, \"16\"],\n[-37.9099549667, 175.46780795, \"17\"],\n[-37.9105899, 175.46737165, \"18A\"],\n[-37.9102941833, 175.4674259333, \"18\"],\n[-37.91020335, 175.4685710333, \"21\"],\n[-37.9102757167, 175.4688396333, \"23\"],\n[-37.9105438167, 175.46821955, \"22\"],\n[-37.9105772667, 175.4685320333, \"26\"],\n[-37.9104076167, 175.4692623833, \"25\"],\n[-37.9094199667, 175.4660506333, \"1\"],\n[-37.9106404167, 175.4687469667, \"28\"],\n[-37.9107060667, 175.4689637167, \"30\"],\n[-37.9103114167, 175.4698414333, \"31\"],\n[-37.9110137167, 175.4690528333, \"34A\"],\n[-37.9107613667, 175.4691689333, \"34\"],\n[-37.91081495, 175.4693474, \"36A\"],\n[-37.9110468333, 175.46924275, \"36B\"],\n[-37.9108736, 175.4695344167, \"38A\"],\n[-37.9111242, 175.46941945, \"38B\"],\n[-37.9098216167, 175.4658393167, \"2\"],\n[-37.91088605, 175.4708887833, \"43\"],\n[-37.9112278167, 175.4705661167, \"44\"],\n[-37.9109336667, 175.47106555, \"45\"],\n[-37.91129165, 175.4707536333, \"46\"],\n[-37.9109882333, 175.4712586667, \"47\"],\n[-37.9113449167, 175.47095045, \"48\"],\n[-37.9110579667, 175.4714307833, \"49\"],\n[-37.9094612667, 175.4662574167, \"3\"],\n[-37.9114052833, 175.4711525333, \"50\"],\n[-37.9114515333, 175.4713499667, \"52\"],\n[-37.9111246333, 175.47187335, \"51\"],\n[-37.91170175, 175.4723085667, \"60\"],\n[-37.9117521667, 175.4724906333, \"62\"],\n[-37.91170015, 175.47359465, \"63\"],\n[-37.91175395, 175.4737979833, \"65\"],\n[-37.9120630333, 175.4735410667, \"66A\"],\n[-37.9122263, 175.47353625, \"66B\"],\n[-37.9118108167, 175.4740034667, \"67\"],\n[-37.9113339, 175.4743958833, \"69\"],\n[-37.90952435, 175.4664596667, \"5\"],\n[-37.90970875, 175.4670344667, \"9\"],\n[-37.91152875, 175.4743488, \"71\"],\n[-37.9123023, 175.47433295, \"72\"],\n[-37.9118344167, 175.4742989833, \"73A\"],\n[-37.9118896667, 175.4742717333, \"73\"],\n[-37.9123685833, 175.4745438, \"74\"],\n[-37.91204505, 175.4747885167, \"77B\"],\n[-37.9124726167, 175.4749395667, \"78\"],\n[-37.9120965167, 175.47493145, \"79\"],\n[-37.9099235167, 175.46615755, \"6\"],\n[-37.9099866, 175.4663651333, \"8\"],\n[-37.9126461, 175.4754722667, \"80\"],\n[-37.9123464833, 175.4758373333, \"81\"],\n[-37.9124222167, 175.4761109333, \"83\"],\n[-37.9129424833, 175.47629755, \"88A\"],\n[-37.91334525, 175.4762585833, \"90A\"],\n[-37.9134976667, 175.4761915, \"90B\"],\n[-37.9133118333, 175.47629305, \"90\"],\n[-37.9127142, 175.4769782833, \"87\"],\n[-37.9127630833, 175.4771701667, \"89\"],\n[-37.9131761, 175.47644985, \"92A\"],\n[-37.9131949667, 175.47653455, \"92B\"],\n[-37.9130077167, 175.4766852167, \"94\"],\n[-37.9131084667, 175.4770053833, \"96\"],\n[-37.9133594333, 175.4770784833, \"98\"],\n[-37.91309155, 175.4760073833, \"86B\"],\n[-37.9127336333, 175.4757125833, \"82\"],\n[-37.9128152667, 175.4759342833, \"84\"],\n[-37.9129075333, 175.4763391333, \"88\"],\n[-37.8906112, 175.4636290667, \"8\"],\n[-37.8906010667, 175.4641357833, \"1A\"],\n[-37.8905902667, 175.4639329, \"4\"],\n[-37.8905940833, 175.4640698, \"2\"],\n[-37.8906316167, 175.4632836667, \"13\"],\n[-37.8907058, 175.4633735833, \"17\"],\n[-37.8905923, 175.4640024833, \"3\"],\n[-37.8907071833, 175.4634437667, \"18\"],\n[-37.8906947667, 175.4635948667, \"20\"],\n[-37.8906874333, 175.4636836833, \"21\"],\n[-37.890681, 175.4637655167, \"22\"],\n[-37.8906765333, 175.4638413833, \"23\"],\n[-37.8906641, 175.4639166833, \"24\"],\n[-37.8906549, 175.4639912667, \"25\"],\n[-37.8906532333, 175.4640676833, \"26\"],\n[-37.8906518667, 175.46414095, \"27\"],\n[-37.8906011167, 175.4638605167, \"5\"],\n[-37.8906079667, 175.4637939167, \"6\"],\n[-37.8906326167, 175.4634340333, \"11\"],\n[-37.8906169667, 175.4635610833, \"9\"],\n[-37.8906290333, 175.46349725, \"10\"],\n[-37.8906352333, 175.4633559333, \"12\"],\n[-37.8906300833, 175.4632139167, \"14\"],\n[-37.8906897167, 175.4632274, \"15\"],\n[-37.8905997667, 175.46420885, \"1\"],\n[-37.8906515667, 175.4642292333, \"28\"],\n[-37.8905993833, 175.4637104667, \"7\"],\n[-37.8905424667, 175.4641317333, \"1B\"],\n[-37.8906997833, 175.4632963667, \"16\"],\n[-37.89069985, 175.4635183333, \"19\"],\n[-37.82488245, 175.3937114333, \"107\"],\n[-37.82511735, 175.3936415, \"105\"],\n[-37.8241600833, 175.3938937, \"115\"],\n[-37.8257229, 175.3934403167, \"95\"],\n[-37.8311935833, 175.39119285, \"34\"],\n[-37.8312837167, 175.3938057, \"44\"],\n[-37.8302991667, 175.3911272833, \"39\"],\n[-37.8298778833, 175.38838305, \"33\"],\n[-37.8260734, 175.3947714167, \"94C\"],\n[-37.8263358833, 175.3938421667, \"94A\"],\n[-37.8279937667, 175.394307, \"72C\"],\n[-37.8246244833, 175.3942836667, \"110\"],\n[-37.82928665, 175.3931185833, \"58\"],\n[-37.8239608833, 175.3939572, \"117\"],\n[-37.8307863167, 175.3929875333, \"46B\"],\n[-37.83183475, 175.3898969167, \"19\"],\n[-37.82823865, 175.3933544333, \"72A\"],\n[-37.82829215, 175.3941929833, \"72B\"],\n[-37.8278144167, 175.3934866833, \"72D\"],\n[-37.8263441833, 175.3946904167, \"94B\"],\n[-37.8258577167, 175.3939724833, \"94D\"],\n[-37.8252182667, 175.3953295167, \"108\"],\n[-37.8301352667, 175.3922486833, \"46A\"],\n[-37.8325430167, 175.3898667667, \"14\"],\n[-37.8327221167, 175.3897138833, \"12\"],\n[-37.91482215, 175.4749911167, \"3\"],\n[-37.9154439833, 175.4755851833, \"12\"],\n[-37.9151334833, 175.4758027, \"13\"],\n[-37.9152983167, 175.4757077333, \"14\"],\n[-37.9150524833, 175.4746568833, \"2\"],\n[-37.9147571833, 175.4747972, \"1\"],\n[-37.9151202833, 175.4748550667, \"4\"],\n[-37.9149427, 175.4758335, \"11\"],\n[-37.91488335, 175.4751902167, \"5\"],\n[-37.9149361833, 175.4753859667, \"7\"],\n[-37.9149649167, 175.475589, \"9\"],\n[-37.9153104333, 175.4754419667, \"10\"],\n[-37.9151775667, 175.4750489, \"6\"],\n[-37.91523395, 175.4752497167, \"8\"],\n[-37.8190687333, 175.3738382833, \"23\"],\n[-37.8189306833, 175.3733703333, \"25\"],\n[-37.8190215667, 175.37468215, \"15\"],\n[-37.8191251667, 175.3740887167, \"21\"],\n[-37.81873305, 175.3726976833, \"27A-27C\"],\n[-37.8806986667, 175.4693562667, \"63\"],\n[-37.8809032, 175.46938395, \"61\"],\n[-37.88419255, 175.4697289167, \"25\"],\n[-37.8840685833, 175.4692688833, \"26A\"],\n[-37.8849148667, 175.4692615167, \"24\"],\n[-37.8847644833, 175.4697682667, \"23\"],\n[-37.8813066833, 175.4696684167, \"55A\"],\n[-37.8841734833, 175.4692622667, \"26\"],\n[-37.8840862833, 175.46972585, \"27\"],\n[-37.883927, 175.46906515, \"28A\"],\n[-37.8838748833, 175.4692541667, \"28\"],\n[-37.8838499, 175.4696946333, \"29\"],\n[-37.88359805, 175.4692251167, \"30\"],\n[-37.8837043833, 175.4696835, \"31\"],\n[-37.8834259333, 175.4692035833, \"32\"],\n[-37.88355905, 175.4696786667, \"33\"],\n[-37.8832160667, 175.4691627833, \"34\"],\n[-37.8834721833, 175.4701433, \"35\"],\n[-37.8829353667, 175.4691514, \"36\"],\n[-37.8833598667, 175.4701280667, \"37\"],\n[-37.8821233833, 175.4690720167, \"44\"],\n[-37.8818720333, 175.4690515667, \"46\"],\n[-37.8816436667, 175.4690230167, \"48\"],\n[-37.8815636667, 175.4686263, \"50\"],\n[-37.8818555333, 175.4695200333, \"51\"],\n[-37.8812979333, 175.4689347, \"52A\"],\n[-37.8814706833, 175.46880395, \"52\"],\n[-37.8816095333, 175.46944925, \"53\"],\n[-37.8816625, 175.4696671833, \"53A\"],\n[-37.8811724833, 175.46894365, \"54\"],\n[-37.8833521, 175.46965845, \"39\"],\n[-37.882727, 175.4691108167, \"40\"],\n[-37.88320755, 175.4696409167, \"41\"],\n[-37.8830678333, 175.4696227833, \"43\"],\n[-37.8828830167, 175.4695893333, \"45\"],\n[-37.8827233167, 175.4695686167, \"47\"],\n[-37.88095035, 175.4689156, \"56\"],\n[-37.8811470667, 175.4694376833, \"57\"],\n[-37.8807286167, 175.4689006167, \"58\"],\n[-37.8809772667, 175.4695997, \"59\"],\n[-37.8855099667, 175.4700903333, \"11A\"],\n[-37.8863533667, 175.4694588667, \"10\"],\n[-37.8855251167, 175.46980435, \"11\"],\n[-37.8858918167, 175.4693764333, \"12\"],\n[-37.88541135, 175.46981195, \"13\"],\n[-37.8857694833, 175.4693831333, \"14\"],\n[-37.8853156333, 175.4697983, \"15\"],\n[-37.88556005, 175.4693647167, \"16\"],\n[-37.8851626, 175.4697849833, \"17\"],\n[-37.8853764833, 175.4693362333, \"18\"],\n[-37.8850294167, 175.4700537333, \"19A\"],\n[-37.8850045667, 175.4697427167, \"19\"],\n[-37.88522955, 175.4692991667, \"20\"],\n[-37.8869080667, 175.4699470333, \"1\"],\n[-37.8866734833, 175.4699262833, \"3\"],\n[-37.8868586667, 175.46949025, \"4\"],\n[-37.8866564167, 175.4694635, \"6\"],\n[-37.8864874833, 175.4690444333, \"8A\"],\n[-37.8864948, 175.4694661333, \"8\"],\n[-37.8857426, 175.4698434167, \"9\"],\n[-37.8813893667, 175.46946355, \"55\"],\n[-37.8757085833, 175.4720279333, \"14\"],\n[-37.8760134667, 175.4732799333, \"4\"],\n[-37.8762548333, 175.4722784667, \"9\"],\n[-37.87627795, 175.47290345, \"5\"],\n[-37.8759331333, 175.4720918833, \"16\"],\n[-37.8759628833, 175.47273515, \"8\"],\n[-37.8762970167, 175.4732576833, \"3\"],\n[-37.8762245833, 175.47260565, \"7\"],\n[-37.8760141333, 175.4735054833, \"2\"],\n[-37.87599385, 175.4730087667, \"6\"],\n[-37.8759230167, 175.4724739333, \"10\"],\n[-37.8762960167, 175.4734868833, \"1\"],\n[-37.8760504333, 175.4719608, \"18\"],\n[-37.8759146, 175.4722788167, \"12\"],\n[-37.8741210833, 175.4688841833, \"4\"],\n[-37.874194, 175.4684628667, \"3\"],\n[-37.8738320667, 175.467738, \"7C\"],\n[-37.8739506833, 175.4677262667, \"7B\"],\n[-37.8738354667, 175.4680554, \"7D\"],\n[-37.8739794167, 175.4680445667, \"7A\"],\n[-37.8738342833, 175.46848195, \"9\"],\n[-37.8740206, 175.4684673, \"5\"],\n[-37.8739604167, 175.4688864, \"6\"],\n[-37.8735957167, 175.4683405333, \"11\"],\n[-37.8734235667, 175.4681024, \"13\"],\n[-37.87339585, 175.4678321167, \"15\"],\n[-37.8733727667, 175.46763935, \"17\"],\n[-37.8728499833, 175.46814045, \"18B\"],\n[-37.8728848, 175.468283, \"18A\"],\n[-37.8730660667, 175.4686787667, \"12B\"],\n[-37.8734160167, 175.4687180167, \"10\"],\n[-37.87312285, 175.4687848, \"12A\"],\n[-37.8730669833, 175.4679829333, \"20\"],\n[-37.8731389833, 175.4682574833, \"16\"],\n[-37.8732649167, 175.4684832833, \"14\"],\n[-37.8732605833, 175.4675842167, \"19\"],\n[-37.8729366833, 175.4674852167, \"24\"],\n[-37.8730152333, 175.4677481333, \"22\"],\n[-37.87312065, 175.4675707, \"21\"],\n[-37.7940869167, 175.4650218, \"53\"],\n[-37.7987550833, 175.46275265, \"10\"],\n[-37.7930822167, 175.4661922833, \"60\"],\n[-37.9811612, 175.51623075, \"170\"],\n[-37.980836, 175.5157299167, \"171\"],\n[-37.9807136667, 175.5162809333, \"172\"],\n[-37.8615888667, 175.47847875, \"25\"],\n[-37.8612666833, 175.4791558, \"31\"],\n[-37.86047605, 175.48172545, \"60\"],\n[-37.8620611333, 175.4774431167, \"13\"],\n[-37.8607505167, 175.4799311167, \"41\"],\n[-37.8611511833, 175.4794658167, \"35\"],\n[-37.86108515, 175.4796282, \"37\"],\n[-37.8604406, 175.48091835, \"55\"],\n[-37.8595911333, 175.483664, \"72\"],\n[-37.8582950167, 175.4794636833, \"1/55\"],\n[-37.8599109167, 175.4805425167, \"49\"],\n[-37.86081285, 175.4790711333, \"33\"],\n[-37.9125544667, 175.4795344, \"6A\"],\n[-37.9129032667, 175.479114, \"8\"],\n[-37.9131012, 175.4790179333, \"10\"],\n[-37.9132466667, 175.4789829167, \"11\"],\n[-37.9130141, 175.4787519333, \"1\"],\n[-37.9128543833, 175.4788087167, \"2\"],\n[-37.9126783167, 175.47889955, \"3\"],\n[-37.9125031333, 175.47905685, \"4\"],\n[-37.91254025, 175.4792764833, \"5\"],\n[-37.9127399333, 175.4793322333, \"7\"],\n[-37.9130969667, 175.47952775, \"9\"],\n[-37.9126911167, 175.4796185, \"6\"],\n[-37.8188045667, 175.445279, \"356B\"],\n[-37.8182903167, 175.4447447333, \"356A\"],\n[-37.8086473833, 175.4195512167, \"27\"],\n[-37.8137567, 175.4426504333, \"299\"],\n[-37.8131862167, 175.4434878167, \"297\"],\n[-37.8157127333, 175.43953285, \"300A\"],\n[-37.8094742167, 175.4189907, \"19\"],\n[-37.80933355, 175.4191978667, \"23\"],\n[-37.8084224833, 175.4207951, \"41\"],\n[-37.8095742167, 175.43605105, \"226\"],\n[-37.81325255, 175.4410722, \"291\"],\n[-37.8149016667, 175.44148445, \"310\"],\n[-37.8155769667, 175.44213865, \"316\"],\n[-37.8161747167, 175.4426252167, \"326A\"],\n[-37.81712155, 175.4417707667, \"326B\"],\n[-37.8169698833, 175.4434206167, \"332\"],\n[-37.81770375, 175.4442239833, \"346\"],\n[-37.81930575, 175.4457899333, \"370\"],\n[-37.82310865, 175.4495740167, \"422\"],\n[-37.8103162667, 175.4344773833, \"224B\"],\n[-37.8100267333, 175.4348309167, \"224A\"],\n[-37.8072155167, 175.4256129167, \"88B\"],\n[-37.8070962167, 175.42278495, \"59\"],\n[-37.8044940333, 175.4270097, \"105\"],\n[-37.8046275667, 175.4276069, \"110\"],\n[-37.8047349, 175.43229485, \"165\"],\n[-37.8073398, 175.4232142167, \"68\"],\n[-37.8064126167, 175.4330381167, \"188\"],\n[-37.8063987833, 175.4339707667, \"209\"],\n[-37.8103206333, 175.4378506833, \"241\"],\n[-37.8117777667, 175.4393874, \"269\"],\n[-37.8074375833, 175.43233545, \"192\"],\n[-37.8099668167, 175.4365891667, \"236\"],\n[-37.8038524, 175.4307279167, \"156\"],\n[-37.808759, 175.4183171167, \"17\"],\n[-37.805885, 175.4246946333, \"81\"],\n[-37.8072272, 175.4225943333, \"57\"],\n[-37.8056120333, 175.4262290333, \"98\"],\n[-37.80400575, 175.4287991167, \"122\"],\n[-37.8162936333, 175.43817215, \"300B\"],\n[-37.80809795, 175.4212756, \"49\"],\n[-37.8102496667, 175.4184802, \"8\"],\n[-37.8057899167, 175.43198855, \"174\"],\n[-37.8060113, 175.4256459333, \"88A\"],\n[-37.8183982, 175.4416558, \"336\"],\n[-37.8108804, 175.4393530667, \"257\"],\n[-37.81106865, 175.4386178, \"255\"],\n[-37.816436, 175.4370654, \"298\"],\n[-37.8152743833, 175.4390058833, \"298A\"],\n[-37.8136452667, 175.44031535, \"288\"],\n[-37.8123581, 175.44091935, \"285\"],\n[-37.81283995, 175.4401099833, \"283\"],\n[-37.8198763333, 175.4462982, \"382\"],\n[-37.8164479167, 175.44390175, \"331\"],\n[-37.80775795, 175.4225746667, \"62\"],\n[-37.8039981833, 175.43163845, \"161\"],\n[-37.81343775, 175.4401047167, \"286\"],\n[-37.92047495, 175.46336425, \"9\"],\n[-37.9203008333, 175.4618285667, \"10\"],\n[-37.9198670667, 175.4617700333, \"6\"],\n[-37.9207259667, 175.4627925167, \"18\"],\n[-37.9204614, 175.4619784833, \"12\"],\n[-37.9205793667, 175.4621705667, \"14\"],\n[-37.9207471167, 175.463665, \"22\"],\n[-37.9208443, 175.4640230833, \"24\"],\n[-37.9207801667, 175.4632019667, \"20\"],\n[-37.919291, 175.4622796167, \"1\"],\n[-37.92066135, 175.4624384833, \"16\"],\n[-37.9203523333, 175.4637463, \"11\"],\n[-37.9192657167, 175.4618642833, \"2\"],\n[-37.9197237333, 175.4622129, \"3\"],\n[-37.91957955, 175.4618272333, \"4\"],\n[-37.9201776333, 175.4622242, \"5\"],\n[-37.9203935, 175.4628145667, \"7\"],\n[-37.9201192, 175.46174985, \"8\"],\n[-37.9031613333, 175.4779369333, \"8\"],\n[-37.90302285, 175.47799245, \"9\"],\n[-37.9033285333, 175.4778655833, \"7\"],\n[-37.90346695, 175.4777957167, \"6B\"],\n[-37.9031769667, 175.4783186167, \"1\"],\n[-37.9034699667, 175.47848295, \"2A\"],\n[-37.90334345, 175.4782583833, \"2\"],\n[-37.9035249833, 175.4781788667, \"3\"],\n[-37.9037982667, 175.4780497167, \"4A\"],\n[-37.9037089833, 175.4781017333, \"4\"],\n[-37.9035266667, 175.4777643667, \"6A\"],\n[-37.9036756833, 175.4778188333, \"5\"],\n[-37.9129985333, 175.4689246333, \"3\"],\n[-37.9132911, 175.4686436333, \"4\"],\n[-37.9127876667, 175.4680122167, \"13\"],\n[-37.9126336667, 175.46802445, \"13A\"],\n[-37.91304995, 175.4678746833, \"10\"],\n[-37.91280555, 175.4681958333, \"11\"],\n[-37.9127504833, 175.4677229167, \"15\"],\n[-37.9133710167, 175.4688942167, \"2\"],\n[-37.9129377, 175.4687422333, \"5\"],\n[-37.9128397333, 175.4683664833, \"9\"],\n[-37.9125584, 175.4677322333, \"15A\"],\n[-37.9132129833, 175.4683870333, \"6\"],\n[-37.9128774667, 175.4685356, \"7\"],\n[-37.9131322833, 175.4681105167, \"8\"],\n[-37.9164839667, 175.4695277167, \"2/205\"],\n[-37.9167949333, 175.4782119333, \"287\"],\n[-37.9086755, 175.4537326833, \"37\"],\n[-37.91527815, 175.4546945333, \"2/88\"],\n[-37.9142658167, 175.4551371, \"1/88\"],\n[-37.9145371, 175.4555229667, \"88\"],\n[-37.9167784167, 175.4698118167, \"209\"],\n[-37.91671315, 175.4695459, \"207\"],\n[-37.916654, 175.4692565, \"203\"],\n[-37.9168566667, 175.4764833167, \"269\"],\n[-37.9170098667, 175.4835371333, \"332\"],\n[-37.9109654167, 175.455206, \"60\"],\n[-37.9170810333, 175.4814135167, \"310\"],\n[-37.9168965667, 175.4758632667, \"263\"],\n[-37.9154102833, 175.46304445, \"149\"],\n[-37.9154961, 175.4634764667, \"153\"],\n[-37.9152487667, 175.4638892333, \"157\"],\n[-37.9156422333, 175.4641169833, \"159\"],\n[-37.9158050167, 175.4648025333, \"165\"],\n[-37.91375215, 175.4584178167, \"106\"],\n[-37.9147696833, 175.4594047833, \"114\"],\n[-37.91474835, 175.46019985, \"121\"],\n[-37.9158845167, 175.4586385833, \"112\"],\n[-37.9150857, 175.459884, \"120\"],\n[-37.9126885833, 175.4573289333, \"82\"],\n[-37.91648165, 175.4694261333, \"1/205\"],\n[-37.9168508333, 175.4701398833, \"211\"],\n[-37.9169940167, 175.4708593833, \"219\"],\n[-37.9170359, 175.4710790167, \"221\"],\n[-37.91708375, 175.4713339, \"223\"],\n[-37.9170401, 175.4721885167, \"231\"],\n[-37.9170336667, 175.4724657667, \"233\"],\n[-37.9169188667, 175.4749939667, \"249\"],\n[-37.9168660833, 175.4760673333, \"265\"],\n[-37.91686435, 175.4768689833, \"271\"],\n[-37.9166818667, 175.4769821, \"273\"],\n[-37.9168443333, 175.4772235167, \"275\"],\n[-37.9166369667, 175.4773580167, \"277\"],\n[-37.9165761667, 175.4777388, \"279\"],\n[-37.9168418833, 175.47757295, \"281\"],\n[-37.9168319167, 175.47791855, \"283\"],\n[-37.9165834833, 175.47804975, \"285\"],\n[-37.91697015, 175.4853758167, \"344\"],\n[-37.9180125667, 175.4811616, \"316\"],\n[-37.9169954833, 175.48221265, \"318\"],\n[-37.9157653333, 175.4543129833, \"3/88\"],\n[-37.9154571667, 175.4552445, \"4/88\"],\n[-37.8827312167, 175.4872892833, \"5\"],\n[-37.8831903667, 175.4877676167, \"8\"],\n[-37.8828024333, 175.4875113, \"7\"],\n[-37.8831045, 175.4875104167, \"6\"],\n[-37.8828241, 175.4877701, \"9\"],\n[-37.88302025, 175.4867592333, \"2\"],\n[-37.8827238667, 175.4870656, \"3\"],\n[-37.8832792333, 175.48802495, \"10\"],\n[-37.88271135, 175.4868632, \"1\"],\n[-37.8830189167, 175.4869722, \"4\"],\n[-37.8828316667, 175.4879782167, \"11\"],\n[-37.8831031667, 175.4879840167, \"12\"],\n[-37.8829747, 175.4879948833, \"13\"],\n[-37.88274605, 175.48484915, \"21\"],\n[-37.8834571667, 175.4855031833, \"4\"],\n[-37.8834955333, 175.4856944167, \"2\"],\n[-37.8838132333, 175.4854477333, \"3\"],\n[-37.8835369833, 175.4848281833, \"11\"],\n[-37.88336725, 175.48485965, \"13\"],\n[-37.8831959833, 175.48485645, \"15\"],\n[-37.8828621167, 175.48476675, \"19\"],\n[-37.8838400833, 175.4849657333, \"7\"],\n[-37.8837638167, 175.4848016167, \"9\"],\n[-37.8829594167, 175.4854216, \"10\"],\n[-37.8827830667, 175.4855451, \"12\"],\n[-37.88274165, 175.4853534, \"14\"],\n[-37.8827611667, 175.4850917, \"16\"],\n[-37.8830644, 175.48501895, \"17\"],\n[-37.8838288, 175.4856380833, \"1\"],\n[-37.8838275, 175.4852247167, \"5\"],\n[-37.883443, 175.4852514667, \"6\"],\n[-37.8831229667, 175.4853610833, \"8\"],\n[-37.8145997, 175.4058182333, \"13\"],\n[-37.8147182333, 175.4078123, \"28\"],\n[-37.81457405, 175.4067919, \"23\"],\n[-37.8140701667, 175.4071422833, \"27\"],\n[-37.8142806667, 175.4076163167, \"29\"],\n[-37.8149311167, 175.4068842833, \"22\"],\n[-37.8150038333, 175.4059631, \"16\"],\n[-37.7951609167, 175.3942458333, \"200\"],\n[-37.7911471333, 175.3929187167, \"159B\"],\n[-37.7905987667, 175.3938164667, \"159C\"],\n[-37.7912155833, 175.3932714167, \"159A\"],\n[-37.79397065, 175.3941075167, \"190\"],\n[-37.7944302667, 175.3943967333, \"196\"],\n[-37.7902842667, 175.3925200667, \"151\"],\n[-37.79289115, 175.3931896, \"174\"],\n[-37.7936966833, 175.3937764, \"188\"],\n[-37.7941318167, 175.3948274667, \"191\"],\n[-37.7978721333, 175.3984214333, \"249\"],\n[-37.7981105667, 175.39574825, \"232A\"],\n[-37.7938025333, 175.3924897, \"178\"],\n[-37.7985639333, 175.3952936333, \"232B\"],\n[-37.7918765, 175.3926438167, \"162\"],\n[-37.7908330333, 175.39227155, \"154\"],\n[-37.7962404, 175.3968673, \"225\"],\n[-37.7923051, 175.3933906333, \"171A\"],\n[-37.7927951833, 175.3942652333, \"171B\"],\n[-37.7901405833, 175.3919332833, \"146\"],\n[-37.7965292833, 175.396445, \"226\"],\n[-37.7989539333, 175.3987508667, \"258\"],\n[-37.7975085, 175.3942399167, \"218A\"],\n[-37.7982403667, 175.3938506167, \"218B\"],\n[-37.7947115833, 175.3953049, \"203\"],\n[-37.7925679167, 175.3935303833, \"171C\"],\n[-37.7951510667, 175.3951699667, \"206\"],\n[-37.7942058167, 175.39426, \"192\"],\n[-37.8151522, 175.3671718, \"2\"],\n[-37.8124476333, 175.3641774667, \"38C\"],\n[-37.8142324833, 175.3640258333, \"36\"],\n[-37.8147477833, 175.3658554333, \"18\"],\n[-37.8149241, 175.3663270333, \"14\"],\n[-37.8127630333, 175.3645959667, \"38D\"],\n[-37.8802473333, 175.4761364833, \"161\"],\n[-37.8803540333, 175.4775673167, \"169A\"],\n[-37.8808955167, 175.4678431, \"87A\"],\n[-37.8812967167, 175.45925645, \"22A\"],\n[-37.8801625833, 175.4788012833, \"181\"],\n[-37.8801798667, 175.4784953, \"179\"],\n[-37.8805060333, 175.4783115333, \"175\"],\n[-37.8802037167, 175.4779421333, \"171\"],\n[-37.8802002, 175.4782253667, \"173\"],\n[-37.88066575, 175.4696257, \"105\"],\n[-37.88142225, 175.4685359, \"99D\"],\n[-37.880149, 175.4790061333, \"183\"],\n[-37.8807033, 175.4684905333, \"99A\"],\n[-37.8811922, 175.4685214167, \"99C\"],\n[-37.8809802667, 175.4685055, \"99B\"],\n[-37.88134225, 175.4657706, \"69\"],\n[-37.8807472167, 175.46754285, \"83\"],\n[-37.8802148333, 175.4776531667, \"169\"],\n[-37.8802820833, 175.4763336, \"163\"],\n[-37.8802558333, 175.4772608667, \"165\"],\n[-37.8806246667, 175.4705465167, \"113\"],\n[-37.88113685, 175.4643511667, \"53A\"],\n[-37.88095905, 175.4643489, \"53\"],\n[-37.88154715, 175.4657577833, \"67\"],\n[-37.8809616167, 175.4639502667, \"49\"],\n[-37.8802025833, 175.48084275, \"191A\"],\n[-37.8806056333, 175.47098625, \"117\"],\n[-37.88044865, 175.47329545, \"127\"],\n[-37.8804726333, 175.47306075, \"125\"],\n[-37.8806984167, 175.4686680667, \"101\"],\n[-37.8805955167, 175.4711419833, \"117A\"],\n[-37.8806145167, 175.4707781333, \"115\"],\n[-37.88164575, 175.45804455, \"16B\"],\n[-37.88036875, 175.4755842, \"153\"],\n[-37.8806508333, 175.4700866667, \"109\"],\n[-37.8815960333, 175.4577537, \"14\"],\n[-37.88151485, 175.4579781667, \"16A\"],\n[-37.8813847833, 175.4644447333, \"55\"],\n[-37.8813670167, 175.4645692667, \"57\"],\n[-37.8809571333, 175.4641156833, \"51\"],\n[-37.8816293, 175.45738225, \"10\"],\n[-37.8818759333, 175.4575147, \"12A\"],\n[-37.8820464833, 175.4575854833, \"12B\"],\n[-37.8820319167, 175.4576747167, \"12C\"],\n[-37.8818586167, 175.4576652167, \"12D\"],\n[-37.8810945667, 175.46145875, \"36\"],\n[-37.8810898, 175.46167115, \"38\"],\n[-37.8810809667, 175.461937, \"40\"],\n[-37.8810701667, 175.4621895167, \"42\"],\n[-37.8812240833, 175.45898385, \"20\"],\n[-37.88121955, 175.45918975, \"22\"],\n[-37.8811737667, 175.45983295, \"24\"],\n[-37.88115855, 175.46004515, \"26\"],\n[-37.8811441, 175.46023075, \"28\"],\n[-37.8814057333, 175.4604056667, \"30\"],\n[-37.8811296167, 175.4604632667, \"32\"],\n[-37.88144325, 175.4580904667, \"18\"],\n[-37.88168545, 175.4564695667, \"2\"],\n[-37.8809346833, 175.4645998167, \"59\"],\n[-37.8809249833, 175.4648314, \"61\"],\n[-37.8810648667, 175.4624290833, \"44\"],\n[-37.8816609833, 175.4570327667, \"8\"],\n[-37.8804975333, 175.4721639167, \"119\"],\n[-37.8804971667, 175.4724005167, \"121\"],\n[-37.8806352, 175.4729319167, \"123A\"],\n[-37.8804951667, 175.4726299167, \"123\"],\n[-37.8806614, 175.4698499833, \"107\"],\n[-37.8806724167, 175.4731077667, \"125A\"],\n[-37.8808698667, 175.47335925, \"129\"],\n[-37.8808554167, 175.4735094333, \"131\"],\n[-37.8804224667, 175.4735332333, \"133\"],\n[-37.8804913, 175.47469655, \"137B\"],\n[-37.8803901, 175.4747040167, \"137\"],\n[-37.8803887333, 175.47486625, \"139\"],\n[-37.8807482167, 175.46714865, \"77A\"],\n[-37.8807597833, 175.4669996167, \"77\"],\n[-37.8807607667, 175.4673298667, \"79\"],\n[-37.88104675, 175.46741105, \"81\"],\n[-37.8812452833, 175.4676640333, \"85\"],\n[-37.8809039167, 175.4650308333, \"63\"],\n[-37.88112735, 175.4652241167, \"65A\"],\n[-37.8808660833, 175.4654164833, \"65\"],\n[-37.8811361, 175.4657563, \"71\"],\n[-37.8808503, 175.4658921167, \"73\"],\n[-37.8808297667, 175.4661495833, \"75\"],\n[-37.8807330167, 175.4677691667, \"87\"],\n[-37.8811556833, 175.46787915, \"89\"],\n[-37.88071555, 175.4680383167, \"91A\"],\n[-37.8807704167, 175.46804005, \"91B\"],\n[-37.88083325, 175.4680391667, \"91C\"],\n[-37.8809986, 175.4680511, \"91\"],\n[-37.8807155167, 175.46826605, \"93\"],\n[-37.8809702167, 175.4682232667, \"95\"],\n[-37.8807815333, 175.4750173333, \"143\"],\n[-37.8803783667, 175.4751021, \"145\"],\n[-37.88035755, 175.4753576667, \"147\"],\n[-37.8807471667, 175.47541785, \"149\"],\n[-37.8807513667, 175.4755767167, \"151\"],\n[-37.8803393, 175.4758476333, \"159\"],\n[-37.8802462333, 175.4774083333, \"167\"],\n[-37.8800620667, 175.4804408, \"189\"],\n[-37.8800557667, 175.48072595, \"191\"],\n[-37.88004135, 175.481018, \"193\"],\n[-37.8807389667, 175.47578795, \"155\"],\n[-37.88007905, 175.4799234833, \"185\"],\n[-37.88086485, 175.4703347667, \"111A\"],\n[-37.8806350333, 175.4703154167, \"111\"],\n[-37.8800789, 175.4801471833, \"187\"],\n[-37.8800045833, 175.4815507667, \"197\"],\n[-37.8800214333, 175.4812237167, \"195\"],\n[-37.8939151167, 175.4701667, \"27\"],\n[-37.8936762, 175.4701298, \"31\"],\n[-37.8924783167, 175.4700222667, \"47\"],\n[-37.8918865167, 175.469973, \"51\"],\n[-37.89367495, 175.4697996667, \"24\"],\n[-37.8943151667, 175.4702028667, \"7\"],\n[-37.8941286667, 175.4702044, \"23\"],\n[-37.8942378667, 175.4698431, \"2\"],\n[-37.8945567833, 175.47024855, \"1/1-3/1\"],\n[-37.8935522667, 175.4697719, \"28\"],\n[-37.8939433, 175.4703330667, \"25\"],\n[-37.8937860667, 175.4701428667, \"29\"],\n[-37.8935538167, 175.4701164833, \"41\"],\n[-37.8941460167, 175.46983875, \"14\"],\n[-37.8938438167, 175.46980825, \"18\"],\n[-37.8937419667, 175.4698073167, \"22\"],\n[-37.8944316, 175.4702330333, \"3\"],\n[-37.8943035167, 175.4704350167, \"9\"],\n[-37.8926233833, 175.4700377333, \"43\"],\n[-37.8922256167, 175.47000115, \"49\"],\n[-37.8917971667, 175.46961, \"54\"],\n[-37.8926854, 175.469645, \"40\"],\n[-37.8921227167, 175.4696225333, \"48\"],\n[-37.8939403167, 175.4698126, \"16\"],\n[-37.8909714667, 175.4681493667, \"33\"],\n[-37.89113605, 175.4681601333, \"31\"],\n[-37.8912736333, 175.4686223167, \"26\"],\n[-37.892202, 175.4686328333, \"16\"],\n[-37.8921055833, 175.46861625, \"18\"],\n[-37.8912406833, 175.4681574833, \"27\"],\n[-37.89073535, 175.4685119833, \"32\"],\n[-37.89037515, 175.4686127333, \"36\"],\n[-37.8907882833, 175.4681261167, \"37-39\"],\n[-37.8905470833, 175.46806415, \"41\"],\n[-37.8928384833, 175.4687398333, \"4\"],\n[-37.8927622667, 175.46885875, \"8\"],\n[-37.8926823333, 175.4687259, \"10\"],\n[-37.89258095, 175.4687121167, \"12\"],\n[-37.89248485, 175.4687019167, \"14\"],\n[-37.9064365333, 175.4541838333, \"29\"],\n[-37.9065554833, 175.4549096667, \"1/35\"],\n[-37.9060750333, 175.4550532833, \"3/35\"],\n[-37.9058351667, 175.4555274667, \"5/35\"],\n[-37.9055143333, 175.4559182333, \"7/35\"],\n[-37.9065810667, 175.4558807167, \"36\"],\n[-37.9065682333, 175.4553069, \"37\"],\n[-37.9064074667, 175.4522252167, \"9\"],\n[-37.9059521, 175.45592445, \"2/35\"],\n[-37.9108997167, 175.4805822667, \"3\"],\n[-37.9109122833, 175.47947205, \"6A\"],\n[-37.9109177167, 175.47936065, \"8\"],\n[-37.9111175, 175.4797094, \"10\"],\n[-37.9113005167, 175.4794321333, \"12\"],\n[-37.9111916333, 175.4793347333, \"12A\"],\n[-37.9112471833, 175.4797754667, \"14\"],\n[-37.9112473667, 175.4800788167, \"11\"],\n[-37.9113031333, 175.479914, \"13\"],\n[-37.91063835, 175.4803100333, \"1\"],\n[-37.9104951, 175.4800418667, \"2\"],\n[-37.9107454333, 175.4798887833, \"4\"],\n[-37.9108743667, 175.4802328833, \"5\"],\n[-37.9109565333, 175.4797847, \"6\"],\n[-37.9110407167, 175.4801576333, \"7\"],\n[-37.9112238167, 175.4804073167, \"9\"],\n[-37.921572, 175.4690554667, \"8\"],\n[-37.9210102667, 175.4691739667, \"1\"],\n[-37.9209054, 175.4687952333, \"2\"],\n[-37.92139265, 175.4695081333, \"3\"],\n[-37.9212156833, 175.4688233667, \"4\"],\n[-37.92161785, 175.4693603333, \"5\"],\n[-37.9215667, 175.4686771667, \"6\"],\n[-37.8971280667, 175.3833620167, \"104\"],\n[-37.8928069667, 175.3849023833, \"58\"],\n[-37.8960316833, 175.3834830167, \"1/84\"],\n[-37.8904228167, 175.38549425, \"28\"],\n[-37.8953458333, 175.3837370167, \"84\"],\n[-37.82145425, 175.3649986833, \"7\"],\n[-37.81972895, 175.3640394833, \"24\"],\n[-37.8211835333, 175.3645971667, \"11\"],\n[-37.8207776167, 175.3647366833, \"14\"],\n[-37.8209467167, 175.3640718167, \"15\"],\n[-37.8203904333, 175.3652387, \"12\"],\n[-37.9137399667, 175.3922778167, \"2/73\"],\n[-37.91124595, 175.39246205, \"46\"],\n[-37.9139008833, 175.3922517333, \"3/73\"],\n[-37.91350815, 175.39233345, \"1/73\"],\n[-37.9176178167, 175.4598785667, \"19\"],\n[-37.9184013833, 175.4603797833, \"14\"],\n[-37.9186543333, 175.4622474833, \"6\"],\n[-37.9173606667, 175.46006885, \"19B\"],\n[-37.9174988, 175.4603532, \"19C\"],\n[-37.9177378167, 175.4601680667, \"19D\"],\n[-37.91904695, 175.4627864167, \"1\"],\n[-37.91872175, 175.46300145, \"2\"],\n[-37.91901275, 175.46243355, \"3\"],\n[-37.9186912, 175.46265345, \"4\"],\n[-37.91856195, 175.4613744167, \"10\"],\n[-37.91851535, 175.4609194, \"12\"],\n[-37.9189516833, 175.4617426667, \"5\"],\n[-37.9188849, 175.4613117333, \"7\"],\n[-37.9186173667, 175.4618227833, \"8\"],\n[-37.9188575667, 175.4608533167, \"9\"],\n[-37.9173323833, 175.4597201333, \"19A\"],\n[-37.9178891, 175.4598711167, \"17\"],\n[-37.9180202833, 175.46025785, \"16\"],\n[-37.9016754833, 175.4689591, \"10\"],\n[-37.9008128667, 175.4686879833, \"2\"],\n[-37.9009759333, 175.46843985, \"1\"],\n[-37.9014464, 175.4685093167, \"5\"],\n[-37.90130095, 175.4688850167, \"6\"],\n[-37.9016211667, 175.4685312667, \"7\"],\n[-37.9014644167, 175.4690226833, \"8\"],\n[-37.9014864167, 175.4687479667, \"9\"],\n[-37.9010720333, 175.4687982333, \"4\"],\n[-37.9011746, 175.4685293333, \"3\"],\n[-37.9013743333, 175.4862629167, \"3\"],\n[-37.9017248, 175.48698725, \"6\"],\n[-37.9012848, 175.4859531833, \"2\"],\n[-37.9016014833, 175.4860586167, \"10\"],\n[-37.9014527667, 175.4865486167, \"4\"],\n[-37.90159855, 175.48677855, \"5\"],\n[-37.90183005, 175.4869517667, \"7\"],\n[-37.9017713833, 175.4866214167, \"8\"],\n[-37.9016935, 175.4863532333, \"9\"],\n[-37.90753, 175.46902475, \"11\"],\n[-37.9095893167, 175.4677886667, \"39B\"],\n[-37.9109792833, 175.4682655, \"46A\"],\n[-37.91108275, 175.4685749667, \"46B\"],\n[-37.91115625, 175.4686338833, \"48C\"],\n[-37.9110050167, 175.46807085, \"48A\"],\n[-37.91095895, 175.46781955, \"48\"],\n[-37.9108204, 175.4678798667, \"46\"],\n[-37.9110788833, 175.4683501, \"48B\"],\n[-37.9113655167, 175.4678862333, \"52B\"],\n[-37.9108344833, 175.4674433667, \"45\"],\n[-37.9080208833, 175.4692374833, \"22\"],\n[-37.9083263167, 175.4686353333, \"27\"],\n[-37.90844025, 175.4697672333, \"26\"],\n[-37.90818025, 175.4691643, \"22A\"],\n[-37.91258895, 175.4673144833, \"64\"],\n[-37.9123684167, 175.4671105167, \"62\"],\n[-37.9127896, 175.4663434833, \"67\"],\n[-37.91150695, 175.46757075, \"54\"],\n[-37.9098419667, 175.4683599167, \"40\"],\n[-37.91097155, 175.4673723, \"47\"],\n[-37.9078617667, 175.4693023667, \"20\"],\n[-37.9089053833, 175.4683338, \"29\"],\n[-37.9099875333, 175.46828695, \"42\"],\n[-37.9113045167, 175.4682282833, \"50A\"],\n[-37.9107373, 175.4665437833, \"47C\"],\n[-37.9108713, 175.4665006167, \"49B\"],\n[-37.9129575833, 175.4665350833, \"69\"],\n[-37.9108057333, 175.4667861, \"47B\"],\n[-37.9108904167, 175.4670609667, \"47A\"],\n[-37.9110041, 175.4668838667, \"49A\"],\n[-37.9111488833, 175.4673072667, \"49\"],\n[-37.9127369333, 175.46653495, \"65\"],\n[-37.91122205, 175.46770665, \"52\"],\n[-37.9113599, 175.467657, \"52A\"],\n[-37.9075002667, 175.4694598, \"12\"],\n[-37.9076741, 175.46939185, \"14\"],\n[-37.9078864833, 175.4699014333, \"16\"],\n[-37.9079676667, 175.46986515, \"18\"],\n[-37.90833225, 175.4695654167, \"24\"],\n[-37.9084313667, 175.4690023, \"28\"],\n[-37.9096631167, 175.46843925, \"38\"],\n[-37.9096189, 175.4680248667, \"39\"],\n[-37.911315, 175.4684015667, \"50\"],\n[-37.9073078333, 175.4691221167, \"9\"],\n[-37.9093386167, 175.4686052333, \"34\"],\n[-37.9091743667, 175.4682295833, \"35\"],\n[-37.90945425, 175.4681136333, \"37\"],\n[-37.90950645, 175.4685246333, \"36\"],\n[-37.9091745333, 175.4686900667, \"32\"],\n[-37.9127273333, 175.4669306667, \"66\"],\n[-37.913043, 175.4669127667, \"68\"],\n[-37.9113586333, 175.4672288167, \"51\"],\n[-37.9113531667, 175.4670099667, \"51A\"],\n[-37.9127951, 175.46667535, \"71\"],\n[-37.884095, 175.4568770667, \"69A\"],\n[-37.8838324333, 175.4568632333, \"73A\"],\n[-37.8866347667, 175.4565719833, \"43\"],\n[-37.8906992, 175.4569129667, \"5\"],\n[-37.8905596667, 175.4568912833, \"7\"],\n[-37.8892758333, 175.456777, \"13A\"],\n[-37.8901881167, 175.45687625, \"1/11-11/11\"],\n[-37.89070845, 175.4565194333, \"1/4-3/4\"],\n[-37.8902009333, 175.4563996667, \"1/6-32/6\"],\n[-37.8880562333, 175.4552566667, \"26A\"],\n[-37.8844905833, 175.4563941, \"65\"],\n[-37.8840861833, 175.45635875, \"69\"],\n[-37.8863777, 175.45685, \"45A\"],\n[-37.8909978167, 175.4569555833, \"3\"],\n[-37.8863633167, 175.4571221667, \"45B\"],\n[-37.8864835667, 175.4565624833, \"45\"],\n[-37.8881257667, 175.4569633333, \"25A\"],\n[-37.8905126667, 175.4574578833, \"5A\"],\n[-37.8907166167, 175.4575558333, \"5B\"],\n[-37.8866317167, 175.4572218, \"41A\"],\n[-37.8873822167, 175.4566324, \"33\"],\n[-37.8872964167, 175.4566407167, \"35\"],\n[-37.8866143833, 175.4569883667, \"41\"],\n[-37.8837334333, 175.45685885, \"75A\"],\n[-37.8860936, 175.4570698167, \"49\"],\n[-37.8855037167, 175.4564867333, \"57\"],\n[-37.8847617833, 175.4564164833, \"59\"],\n[-37.8844513167, 175.4566735333, \"65A\"],\n[-37.88419435, 175.45689485, \"67A\"],\n[-37.8842829333, 175.4563860333, \"67\"],\n[-37.88391675, 175.4563527, \"73\"],\n[-37.8837116667, 175.4563469333, \"75\"],\n[-37.889394, 175.4567865833, \"13\"],\n[-37.8890782333, 175.4567699333, \"15\"],\n[-37.8888433833, 175.4567615833, \"17\"],\n[-37.8886293333, 175.45674825, \"19\"],\n[-37.8884455333, 175.4567414833, \"21\"],\n[-37.8882809333, 175.45673085, \"23\"],\n[-37.8881163667, 175.456655, \"25\"],\n[-37.8880094667, 175.4562455667, \"26\"],\n[-37.89142225, 175.4569765, \"1\"],\n[-37.8914418, 175.4565121, \"2\"],\n[-37.8870694, 175.4566186333, \"37\"],\n[-37.8868689667, 175.45709495, \"39A\"],\n[-37.8868342167, 175.4565953333, \"39\"],\n[-37.8863517167, 175.4573732167, \"45C\"],\n[-37.8861983667, 175.4570712667, \"47A\"],\n[-37.8862286, 175.4568411833, \"47B\"],\n[-37.8880125833, 175.4547576833, \"26B\"],\n[-37.8860945833, 175.4565352167, \"49A\"],\n[-37.8904199167, 175.4568860167, \"1/9-12/9\"],\n[-37.8857833, 175.4565171833, \"53\"],\n[-37.8856455167, 175.45651885, \"55\"],\n[-37.8817373833, 175.4556858833, \"72\"],\n[-37.88593655, 175.45653005, \"51\"],\n[-37.8862765833, 175.4565127, \"47\"],\n[-37.8775379833, 175.4825127167, \"28A\"],\n[-37.87727315, 175.482376, \"28\"],\n[-37.87729155, 175.4821068667, \"26\"],\n[-37.8769019667, 175.4786293, \"4C\"],\n[-37.8767004333, 175.4787606, \"4B\"],\n[-37.8772910833, 175.4818029333, \"24\"],\n[-37.8771890667, 175.48153255, \"22\"],\n[-37.87708045, 175.4812278, \"20\"],\n[-37.8775351, 175.4826397667, \"30A\"],\n[-37.8772534167, 175.4827040167, \"30\"],\n[-37.877579, 175.4818970667, \"24A\"],\n[-37.876712, 175.4812017667, \"21\"],\n[-37.8773884333, 175.4832404167, \"36\"],\n[-37.8769804167, 175.4824093, \"27\"],\n[-37.8766636667, 175.482704, \"31B\"],\n[-37.8769937167, 175.4830107167, \"31\"],\n[-37.8767516667, 175.479482, \"8A\"],\n[-37.8769705667, 175.4818533667, \"23\"],\n[-37.8766017167, 175.48246365, \"29A\"],\n[-37.8764641833, 175.4825834, \"29B\"],\n[-37.8769964667, 175.4820980833, \"25\"],\n[-37.8769606333, 175.4826863833, \"29\"],\n[-37.87679475, 175.4828674667, \"31A\"],\n[-37.8770826, 175.4832520333, \"33\"],\n[-37.8772377, 175.48334835, \"35\"],\n[-37.8775367167, 175.48363695, \"36A\"],\n[-37.8775890833, 175.4831116, \"34\"],\n[-37.8773408667, 175.4829609, \"32\"],\n[-37.8762828667, 175.4801149667, \"11\"],\n[-37.87597055, 175.4792251167, \"3\"],\n[-37.8760466167, 175.4794375833, \"5\"],\n[-37.8761314, 175.4796617, \"7\"],\n[-37.8764699833, 175.48058085, \"15\"],\n[-37.8765455167, 175.4808159833, \"17\"],\n[-37.8767292667, 175.48028335, \"14\"],\n[-37.8765539833, 175.4798159833, \"10\"],\n[-37.87646245, 175.4795675, \"8\"],\n[-37.8763712167, 175.4793057167, \"6\"],\n[-37.8776228167, 175.481266, \"20B\"],\n[-37.8775729667, 175.482055, \"26A\"],\n[-37.8776195833, 175.4814085333, \"22B\"],\n[-37.8765414, 175.4790666667, \"6A\"],\n[-37.8773585333, 175.48356815, \"38A\"],\n[-37.8762120667, 175.4798686667, \"9\"],\n[-37.8766452667, 175.4800751167, \"12\"],\n[-37.8763675, 175.48034475, \"13\"],\n[-37.8766347833, 175.4810402667, \"19\"],\n[-37.8774052167, 175.4812147167, \"20A\"],\n[-37.8774057667, 175.4814070167, \"22A\"],\n[-37.8762723167, 175.479009, \"4\"],\n[-37.8761971167, 175.4787850833, \"2\"],\n[-37.8767849, 175.4796004667, \"10A\"],\n[-37.87648555, 175.47892075, \"4A\"],\n[-37.8767848, 175.4788595833, \"6B\"],\n[-37.8997092333, 175.48180175, \"7\"],\n[-37.8999472667, 175.4825543, \"15\"],\n[-37.8999358167, 175.4815958167, \"4\"],\n[-37.9000434833, 175.4825363833, \"17\"],\n[-37.9002215333, 175.4828014167, \"16\"],\n[-37.90002095, 175.4818578, \"6\"],\n[-37.9000853167, 175.48210025, \"8\"],\n[-37.8997820833, 175.4820087333, \"9\"],\n[-37.8998240167, 175.4822147833, \"11\"],\n[-37.8998576333, 175.48247455, \"13\"],\n[-37.8996422833, 175.4815878833, \"5\"],\n[-37.9002942833, 175.482764, \"14\"],\n[-37.9001676167, 175.4824792667, \"12\"],\n[-37.89953345, 175.4811912333, \"1\"],\n[-37.8997866833, 175.4810307833, \"2\"],\n[-37.8995802333, 175.4813792333, \"3\"],\n[-37.9001401333, 175.4822942333, \"10\"],\n[-37.8914648833, 175.4772108167, \"7\"],\n[-37.8919011833, 175.47723515, \"1\"],\n[-37.89187105, 175.47681175, \"2\"],\n[-37.8913584, 175.4765354167, \"3/6\"],\n[-37.8897734833, 175.47664395, \"22\"],\n[-37.8913508, 175.47668445, \"1/6\"],\n[-37.8915273667, 175.4762602, \"12/6\"],\n[-37.8916285, 175.4772315333, \"5\"],\n[-37.8913796, 175.4761122167, \"7/6\"],\n[-37.8915023833, 175.4767099333, \"8/6\"],\n[-37.89152, 175.4763519333, \"11/6\"],\n[-37.8915372, 175.4760592333, \"14/6\"],\n[-37.89153065, 175.4761616667, \"13/6\"],\n[-37.8915142333, 175.4764663667, \"10/6\"],\n[-37.8913717333, 175.4763199333, \"5/6\"],\n[-37.8905254833, 175.4766904, \"14\"],\n[-37.8903609333, 175.4766834833, \"16\"],\n[-37.88999105, 175.4766566833, \"20\"],\n[-37.8898741167, 175.4770700667, \"21\"],\n[-37.88946535, 175.4766037667, \"24\"],\n[-37.8890790333, 175.4764705833, \"28\"],\n[-37.88883405, 175.4767525667, \"29\"],\n[-37.8886383, 175.47663365, \"31\"],\n[-37.8913739667, 175.47622135, \"6/6\"],\n[-37.8913652833, 175.47641835, \"4/6\"],\n[-37.8901779667, 175.4766671167, \"18\"],\n[-37.8843281, 175.4794198167, \"104\"],\n[-37.88535045, 175.4866782167, \"184\"],\n[-37.8854485333, 175.4864475167, \"182\"],\n[-37.8867971833, 175.4705084167, \"34\"],\n[-37.8873998833, 175.4682775833, \"20\"],\n[-37.8873415667, 175.46844545, \"22\"],\n[-37.884267, 175.4871188333, \"189\"],\n[-37.8843410333, 175.4813043, \"114A\"],\n[-37.8836708667, 175.47833265, \"90A\"],\n[-37.88402285, 175.4784176667, \"2/92\"],\n[-37.8841246, 175.4784325, \"1/92\"],\n[-37.8874953667, 175.4680249333, \"18\"],\n[-37.8882076333, 175.4665346667, \"2\"],\n[-37.8847749, 175.4777212667, \"89\"],\n[-37.8847129167, 175.4883360167, \"198\"],\n[-37.8852412667, 175.4950174167, \"258\"],\n[-37.8851558333, 175.4809135167, \"111A\"],\n[-37.88425915, 175.490565, \"219\"],\n[-37.8843227167, 175.4792336667, \"102\"],\n[-37.8853703333, 175.47613215, \"70\"],\n[-37.8841992667, 175.4963818333, \"267\"],\n[-37.8839456333, 175.47862415, \"96\"],\n[-37.8849404833, 175.4785609167, \"91B\"],\n[-37.8834439167, 175.5003043667, \"309\"],\n[-37.8859648333, 175.4880242333, \"2/194\"],\n[-37.88679165, 175.4708238833, \"36\"],\n[-37.8846928333, 175.4966356667, \"270\"],\n[-37.8843265, 175.4790500333, \"100\"],\n[-37.8847626167, 175.48078795, \"109\"],\n[-37.88474565, 175.4788347167, \"93\"],\n[-37.8843263, 175.4786463833, \"94\"],\n[-37.8847555167, 175.4792519167, \"95\"],\n[-37.8847507833, 175.4802669333, \"103\"],\n[-37.8877261667, 175.4675846667, \"1/14-8/14\"],\n[-37.884759, 175.4805435333, \"105\"],\n[-37.8843192, 175.4802557667, \"108\"],\n[-37.8847632167, 175.4813335333, \"113\"],\n[-37.8880664667, 175.4668566667, \"4\"],\n[-37.8843192833, 175.4781745833, \"88\"],\n[-37.8842179667, 175.4927192667, \"239\"],\n[-37.8842136333, 175.4948947333, \"257\"],\n[-37.8841869, 175.4971351, \"275\"],\n[-37.8842592167, 175.487337, \"191\"],\n[-37.8842746, 175.4896256667, \"213\"],\n[-37.8842726, 175.4898100833, \"1/213\"],\n[-37.8846939167, 175.49589215, \"264\"],\n[-37.8847557667, 175.480013, \"101\"],\n[-37.8847654333, 175.4817589833, \"117\"],\n[-37.8843088167, 175.4819155, \"120\"],\n[-37.88495845, 175.4760208167, \"74A\"],\n[-37.8858546833, 175.48954395, \"1/214\"],\n[-37.8847226333, 175.49078585, \"222\"],\n[-37.8853824333, 175.4904875333, \"220\"],\n[-37.8851609, 175.4769312833, \"75\"],\n[-37.8832135333, 175.4966401667, \"273\"],\n[-37.8871125333, 175.4687638667, \"24A\"],\n[-37.8836533833, 175.49965535, \"303\"],\n[-37.8847623833, 175.4784197, \"91\"],\n[-37.8853003167, 175.4812587, \"113B\"],\n[-37.8839543333, 175.4780861333, \"86\"],\n[-37.8846811667, 175.49516135, \"260\"],\n[-37.8856284667, 175.4877555167, \"1/194\"],\n[-37.88470535, 175.4899576833, \"1/218\"],\n[-37.8858448, 175.4898961, \"2/218\"],\n[-37.8857487667, 175.4882389667, \"3/194\"],\n[-37.8858920833, 175.4886561833, \"4/194\"],\n[-37.8854131167, 175.4885925, \"202\"],\n[-37.8852231333, 175.48536035, \"174\"],\n[-37.88721395, 175.4674219167, \"12A\"],\n[-37.8850944, 175.4812663333, \"113A\"],\n[-37.8858759667, 175.4852820333, \"172\"],\n[-37.8843297167, 175.4777121167, \"80\"],\n[-37.8867259833, 175.4716590667, \"42\"],\n[-37.8841291333, 175.4812217833, \"114\"],\n[-37.8878423667, 175.46733415, \"10\"],\n[-37.8873731, 175.46722755, \"12\"],\n[-37.8873645167, 175.4677444, \"16A\"],\n[-37.8873792, 175.4676194833, \"16B\"],\n[-37.8871674667, 175.4675664, \"16C\"],\n[-37.8871631333, 175.4677062, \"16D\"],\n[-37.88761885, 175.4678307833, \"16\"],\n[-37.8872513, 175.4677940833, \"18A\"],\n[-37.8868627833, 175.4703096167, \"32\"],\n[-37.8866729333, 175.4725751667, \"46\"],\n[-37.8866649333, 175.4727545667, \"48\"],\n[-37.8866508833, 175.4729862833, \"50\"],\n[-37.8866308167, 175.4733044333, \"52\"],\n[-37.8866201167, 175.4735912, \"54\"],\n[-37.8866046, 175.4738994833, \"56\"],\n[-37.8867646333, 175.4710940833, \"38\"],\n[-37.8867434167, 175.4713738, \"40\"],\n[-37.8865795333, 175.471651, \"42A\"],\n[-37.8867192167, 175.4718726833, \"44\"],\n[-37.8879124167, 175.4671744, \"8\"],\n[-37.8847677333, 175.48104275, \"111\"],\n[-37.8842908, 175.4810946667, \"112\"],\n[-37.8847655167, 175.48152025, \"115\"],\n[-37.8842992333, 175.4815339, \"116\"],\n[-37.8843125167, 175.4817539167, \"118\"],\n[-37.8847685333, 175.4795402167, \"97\"],\n[-37.8847615667, 175.4797710333, \"99\"],\n[-37.8849925167, 175.48009295, \"101A\"],\n[-37.8850730167, 175.4803513167, \"103A\"],\n[-37.8850517667, 175.4806989167, \"107\"],\n[-37.8842659667, 175.4776982667, \"80A\"],\n[-37.8843255167, 175.4779083833, \"82\"],\n[-37.88372445, 175.4779537667, \"84A\"],\n[-37.88391925, 175.4779436333, \"84\"],\n[-37.8839372667, 175.4783140167, \"90\"],\n[-37.8849343833, 175.47834135, \"91A\"],\n[-37.8843215, 175.47842775, \"92\"],\n[-37.88379995, 175.4786243333, \"96A\"],\n[-37.8843308167, 175.47886585, \"98\"],\n[-37.8846524333, 175.4766493167, \"76\"],\n[-37.8845290833, 175.4764747667, \"78A\"],\n[-37.8845141833, 175.47672525, \"78\"],\n[-37.8847249, 175.4874558833, \"192\"],\n[-37.88422795, 175.4880656333, \"197\"],\n[-37.8842586167, 175.4888764, \"205\"],\n[-37.8847152, 175.4891847, \"206\"],\n[-37.8847452333, 175.48597775, \"180\"],\n[-37.8842725667, 175.4862342167, \"181\"],\n[-37.8842734167, 175.4864492, \"183\"],\n[-37.88427415, 175.4866713, \"185\"],\n[-37.8847306167, 175.48657095, \"186\"],\n[-37.88426915, 175.48689685, \"187\"],\n[-37.8842773667, 175.4856398, \"179\"],\n[-37.8852212833, 175.4924678, \"236\"],\n[-37.8854883167, 175.4880036333, \"194\"],\n[-37.8854214333, 175.48956085, \"212\"],\n[-37.8853153667, 175.4872245, \"190\"],\n[-37.8841928667, 175.4931503833, \"241\"],\n[-37.8844711667, 175.4984144, \"290\"],\n[-37.8842522667, 175.4992926333, \"296\"],\n[-37.88368415, 175.4993789667, \"299\"],\n[-37.8852287833, 175.49615515, \"268\"],\n[-37.8859148667, 175.4897217, \"214\"],\n[-37.8872887667, 175.4686820167, \"24\"],\n[-37.8862459167, 175.47553185, \"66\"],\n[-37.884721, 175.48537045, \"176\"],\n[-37.88427605, 175.4851847833, \"175\"],\n[-37.8842737833, 175.4854139833, \"177\"],\n[-37.88469795, 175.4945610167, \"252\"],\n[-37.8853844667, 175.47677695, \"73\"],\n[-37.8850111333, 175.4763984, \"74\"],\n[-37.8847382167, 175.4869962333, \"188\"],\n[-37.8872426333, 175.4689359833, \"26\"],\n[-37.8847101833, 175.4880854667, \"196\"],\n[-37.8850733, 175.4902483167, \"218\"],\n[-37.8865769333, 175.4741854167, \"58\"],\n[-37.8865507, 175.4747159333, \"62\"],\n[-37.88798625, 175.46700445, \"6\"],\n[-37.8865659, 175.4744709833, \"60\"],\n[-37.8865476667, 175.4748834667, \"64\"],\n[-37.8871864333, 175.4691891, \"28\"],\n[-37.8871582, 175.4693788167, \"30\"],\n[-37.8869467833, 175.4687736667, \"26A\"],\n[-37.8856446167, 175.4771148167, \"71\"],\n[-37.8851925333, 175.4762547833, \"72\"],\n[-37.8869292167, 175.4689437, \"26B\"],\n[-37.88426605, 175.48474735, \"171\"],\n[-37.884281, 175.4849628667, \"173\"],\n[-37.8847245167, 175.4848834667, \"170\"],\n[-37.8680607667, 175.4844529, \"318\"],\n[-37.86322315, 175.477174, \"229\"],\n[-37.86023885, 175.47171045, \"177\"],\n[-37.86190065, 175.4739293333, \"200\"],\n[-37.8673314333, 175.4835841, \"306\"],\n[-37.8671325167, 175.4839981167, \"307\"],\n[-37.8657566333, 175.4825140333, \"287\"],\n[-37.85661605, 175.4580103167, \"35\"],\n[-37.8584986667, 175.4693192667, \"151\"],\n[-37.86640715, 175.4833253333, \"293\"],\n[-37.8662972, 175.48401185, \"301\"],\n[-37.8615068, 175.4740312667, \"197\"],\n[-37.86248105, 175.4746844667, \"208\"],\n[-37.8654257333, 175.4789157833, \"256\"],\n[-37.86815405, 175.4856843833, \"327\"],\n[-37.8559617833, 175.4538523, \"5\"],\n[-37.8560217167, 175.4541485167, \"7\"],\n[-37.8545882, 175.4628883167, \"73\"],\n[-37.8623530667, 175.4756642667, \"211\"],\n[-37.8571193833, 175.46393905, \"91\"],\n[-37.8612014833, 175.47249975, \"184\"],\n[-37.8409288167, 175.37611985, \"48\"],\n[-37.8396866, 175.3762834833, \"52\"],\n[-37.840238, 175.3744657333, \"75\"],\n[-37.84304635, 175.3783802167, \"24\"],\n[-37.8407690333, 175.3753619333, \"59\"],\n[-37.8447812333, 175.3774333333, \"5\"],\n[-37.83956405, 175.3732069667, \"82\"],\n[-37.84004855, 175.3731001333, \"83\"],\n[-37.8401183167, 175.3739726667, \"77B\"],\n[-37.8406453, 175.3751587333, \"63\"],\n[-37.8435605667, 175.3793269333, \"20\"],\n[-37.8417591667, 175.3760676667, \"49\"],\n[-37.8413693, 175.3763867167, \"46\"],\n[-37.8399756833, 175.3748775, \"74\"],\n[-37.8421274167, 175.3758927833, \"47\"],\n[-37.8433708, 175.37858675, \"22\"],\n[-37.8410302333, 175.3737431833, \"77A\"],\n[-37.7912387167, 175.4809981333, \"33\"],\n[-37.7921037167, 175.48244855, \"34\"],\n[-37.7931116833, 175.4894669167, \"108\"],\n[-37.7922422833, 175.4888664333, \"109\"],\n[-37.8904655, 175.4340570833, \"16\"],\n[-37.8912684167, 175.4347386333, \"38\"],\n[-37.8918202833, 175.4356853333, \"41\"],\n[-37.9076504833, 175.4808292167, \"2\"],\n[-37.9077122167, 175.4810869667, \"3\"],\n[-37.9078427, 175.4811889667, \"4\"],\n[-37.9079905333, 175.4810739167, \"5\"],\n[-37.9079955, 175.4808534333, \"6\"],\n[-37.9078875833, 175.48062165, \"7\"],\n[-37.8624973167, 175.3995043333, \"1302\"],\n[-37.86458815, 175.3984334333, \"1302B\"],\n[-37.8495279667, 175.3855142, \"1105\"],\n[-37.8409388, 175.3715485333, \"965\"],\n[-37.8497607833, 175.3787181667, \"1048D\"],\n[-37.8474886167, 175.3815213, \"1071\"],\n[-37.8692470333, 175.4060449833, \"1399\"],\n[-37.86903025, 175.4050720333, \"1410\"],\n[-37.8720073, 175.4080915167, \"1435\"],\n[-37.8472647, 175.3801392333, \"1050\"],\n[-37.8484202, 175.37745075, \"1048B\"],\n[-37.8501358167, 175.37656745, \"1048C\"],\n[-37.8464420667, 175.3786923167, \"1048A\"],\n[-37.8636343667, 175.3989099167, \"1302A\"],\n[-37.86024445, 175.39640055, \"1262B\"],\n[-37.8455550167, 175.3768668, \"1032\"],\n[-37.8498453667, 175.3860935167, \"1113\"],\n[-37.8508258833, 175.3877938167, \"1129\"],\n[-37.8538246333, 175.3925789833, \"1193\"],\n[-37.8560212667, 175.3943255167, \"1209\"],\n[-37.8600963167, 175.39687755, \"1262A\"],\n[-37.8619671667, 175.398902, \"1292\"],\n[-37.8647795167, 175.4015556333, \"1328A\"],\n[-37.8641711, 175.40119575, \"1328B\"],\n[-37.8653665667, 175.4021896833, \"1328\"],\n[-37.8664159, 175.4057836, \"1343\"],\n[-37.8487010667, 175.3828026833, \"1084B\"],\n[-37.8487082, 175.3838392667, \"1095\"],\n[-37.84854995, 175.3826141833, \"1084A\"],\n[-37.8614933, 175.3962749667, \"1262C\"],\n[-37.8941389333, 175.4685426667, \"1A\"],\n[-37.8940808333, 175.46863395, \"1C\"],\n[-37.8941101167, 175.46858455, \"1B\"],\n[-37.8828690833, 175.4756987833, \"2\"],\n[-37.8829161167, 175.4758464333, \"3\"],\n[-37.8828703333, 175.47600365, \"4\"],\n[-37.8826165, 175.4759774167, \"5\"],\n[-37.8827374, 175.4756385167, \"1\"],\n[-37.8987844, 175.5778468, \"196\"],\n[-37.894548, 175.5752196167, \"249\"],\n[-37.8985254833, 175.5770166167, \"3/207\"],\n[-37.8979621333, 175.5725676167, \"1/207\"],\n[-37.9028557167, 175.5775770667, \"1/159\"],\n[-37.8968063833, 175.5714292167, \"2/209\"],\n[-37.8982935667, 175.5770144167, \"4/207\"],\n[-37.9069320167, 175.5687670833, \"57\"],\n[-37.89817425, 175.5708251167, \"2/207\"],\n[-37.90506995, 175.5738151167, \"103\"],\n[-37.9103484333, 175.57042275, \"15\"],\n[-37.8938830167, 175.57586675, \"1/253\"],\n[-37.8936416667, 175.5760817333, \"2/253\"],\n[-37.90255875, 175.5776407833, \"2/159\"],\n[-37.9004225167, 175.57797105, \"178\"],\n[-37.8969604, 175.5723327, \"1/209\"],\n[-37.8767003833, 175.4818637833, \"31\"],\n[-37.8757476333, 175.4817359, \"21A\"],\n[-37.87485205, 175.4797409167, \"8\"],\n[-37.87404295, 175.47989395, \"5B\"],\n[-37.8752598667, 175.4811949, \"15A\"],\n[-37.8760220833, 175.4807411167, \"18A\"],\n[-37.8743084667, 175.4802727, \"7A\"],\n[-37.8738682667, 175.4797568167, \"3C\"],\n[-37.8742434333, 175.47987315, \"5A\"],\n[-37.8759584333, 175.4806079667, \"16A\"],\n[-37.87510295, 175.48015575, \"12\"],\n[-37.8748438833, 175.48075465, \"11A\"],\n[-37.8763291167, 175.4819017667, \"27\"],\n[-37.8763574333, 175.4822859833, \"29A\"],\n[-37.8751449333, 175.48070945, \"13\"],\n[-37.8761885667, 175.4822191333, \"27A\"],\n[-37.8746679833, 175.4804258667, \"9A\"],\n[-37.87496675, 175.4808899333, \"13A\"],\n[-37.8757570833, 175.4808296833, \"16\"],\n[-37.8758986833, 175.4809562833, \"18\"],\n[-37.87612285, 175.4811980333, \"20\"],\n[-37.87624255, 175.4813098333, \"22\"],\n[-37.8748282667, 175.47948305, \"4\"],\n[-37.875127, 175.47954055, \"6\"],\n[-37.8765282333, 175.4819416667, \"29\"],\n[-37.87591735, 175.48192895, \"23\"],\n[-37.8759485667, 175.4815328667, \"21\"],\n[-37.87574945, 175.4813294167, \"19\"],\n[-37.8742483167, 175.4796583833, \"3A\"],\n[-37.8745132, 175.4795744833, \"3\"],\n[-37.8744832833, 175.4792603167, \"1\"],\n[-37.8745364667, 175.4798557, \"5\"],\n[-37.8746295, 175.4801628, \"7\"],\n[-37.8747969, 175.480295, \"9\"],\n[-37.8745014333, 175.48041985, \"9B\"],\n[-37.8740439, 175.4796883833, \"3B\"],\n[-37.8764163833, 175.48148435, \"24\"],\n[-37.8753753333, 175.4813299167, \"17A\"],\n[-37.8749682667, 175.4800068, \"10\"],\n[-37.8761475667, 175.481741, \"25\"],\n[-37.8755644, 175.4811591833, \"17\"],\n[-37.8753512333, 175.4809225, \"15\"],\n[-37.87494615, 175.4804919833, \"11\"],\n[-37.87563925, 175.4816151667, \"19A\"],\n[-37.8755416, 175.4806192667, \"14\"],\n[-37.9307413833, 175.4305783833, \"8\"],\n[-37.9316973167, 175.4500289833, \"174\"],\n[-37.9312246333, 175.4509272167, \"183\"],\n[-37.9317041167, 175.4519870667, \"192\"],\n[-37.9309645833, 175.4455487, \"131\"],\n[-37.9313707333, 175.4417409833, \"106\"],\n[-37.93144905, 175.4439987167, \"124\"],\n[-37.9312471833, 175.44044115, \"90\"],\n[-37.9310525167, 175.4572123333, \"249\"],\n[-37.9304025833, 175.4600948, \"271\"],\n[-37.9313988167, 175.4667548167, \"330\"],\n[-37.9317416, 175.4711342333, \"370\"],\n[-37.9311057, 175.4496502333, \"171\"],\n[-37.9307743333, 175.46711525, \"333\"],\n[-37.8983426, 175.4726432167, \"6\"],\n[-37.8982667833, 175.4723224167, \"10\"],\n[-37.8981903, 175.4719677667, \"14\"],\n[-37.8981034667, 175.4717559, \"16\"],\n[-37.8979483667, 175.47163995, \"18\"],\n[-37.8978498167, 175.4718887667, \"9\"],\n[-37.89804025, 175.4727496, \"1\"],\n[-37.8978692667, 175.4717117333, \"11\"],\n[-37.8982294833, 175.4721665, \"12\"],\n[-37.8983239333, 175.4724718833, \"8\"],\n[-37.89833275, 175.4728329, \"4\"],\n[-37.89817045, 175.4730263833, \"2\"],\n[-37.8979749667, 175.4721439333, \"7\"],\n[-37.8978645167, 175.4723602833, \"5\"],\n[-37.8980527833, 175.4724426, \"3\"],\n[-37.9118492167, 175.4777301333, \"75\"],\n[-37.9071599667, 175.48024025, \"27A\"],\n[-37.9070499833, 175.4802823167, \"27\"],\n[-37.9133935, 175.477312, \"101\"],\n[-37.9109577667, 175.4792493167, \"58A\"],\n[-37.9079175667, 175.4798474167, \"35\"],\n[-37.9057049167, 175.48129845, \"18\"],\n[-37.9045409833, 175.4821761, \"8A\"],\n[-37.9132981167, 175.47734815, \"99\"],\n[-37.9151731167, 175.4762851667, \"115A\"],\n[-37.9159149667, 175.4765031667, \"128A\"],\n[-37.9160620833, 175.4770954, \"126\"],\n[-37.91362925, 175.4775332333, \"96\"],\n[-37.9161206833, 175.47737745, \"124\"],\n[-37.9138744167, 175.4774362833, \"98\"],\n[-37.9159982167, 175.4768568, \"126A\"],\n[-37.9045271167, 175.4818595, \"8\"],\n[-37.9088441167, 175.4798303833, \"40\"],\n[-37.9080257833, 175.4798009667, \"37\"],\n[-37.9118080833, 175.4780394667, \"73\"],\n[-37.9120441667, 175.4783650167, \"80\"],\n[-37.9142880667, 175.4768798167, \"107B\"],\n[-37.9159508833, 175.4774919167, \"122A\"],\n[-37.9050646333, 175.4819231333, \"14A\"],\n[-37.9066002, 175.4808875, \"28A\"],\n[-37.90519225, 175.4815441667, \"16\"],\n[-37.91452115, 175.477165, \"106\"],\n[-37.90746095, 175.4804961333, \"32\"],\n[-37.91248555, 175.47814135, \"86\"],\n[-37.9158230167, 175.4760962167, \"123\"],\n[-37.9159919, 175.47604385, \"125\"],\n[-37.91623445, 175.4763621, \"130\"],\n[-37.90872535, 175.4791772, \"41A\"],\n[-37.9044587, 175.4821471167, \"6A\"],\n[-37.9075026667, 175.4797328167, \"33A\"],\n[-37.9109141167, 175.4788989667, \"58\"],\n[-37.9048600833, 175.4817091833, \"12\"],\n[-37.9156691333, 175.4761731833, \"121\"],\n[-37.9159156667, 175.4772401333, \"120\"],\n[-37.9158559667, 175.4770386667, \"118\"],\n[-37.9153897167, 175.4763100667, \"117\"],\n[-37.9155261333, 175.4762367333, \"119\"],\n[-37.9143680667, 175.4772159833, \"104\"],\n[-37.9091816833, 175.4799197, \"44A\"],\n[-37.9063697833, 175.4811747, \"26B\"],\n[-37.9055854667, 175.4813526667, \"18B\"],\n[-37.9140346667, 175.4774023333, \"100\"],\n[-37.9160233167, 175.4772866833, \"122B\"],\n[-37.9046896833, 175.4817846667, \"10\"],\n[-37.9078630333, 175.4802907167, \"34\"],\n[-37.9074586667, 175.4800447667, \"31\"],\n[-37.90766965, 175.4799655333, \"33\"],\n[-37.9121775667, 175.4782823667, \"82\"],\n[-37.91233785, 175.4782158167, \"84\"],\n[-37.9135061167, 175.47759595, \"92\"],\n[-37.9141940333, 175.4769194, \"107A\"],\n[-37.9047462167, 175.4820914167, \"10A\"],\n[-37.9071456333, 175.4806193333, \"30\"],\n[-37.9126591, 175.4780602, \"88\"],\n[-37.9043394833, 175.4819642167, \"6\"],\n[-37.9136969, 175.47784185, \"96A\"],\n[-37.9162752167, 175.4761951167, \"132\"],\n[-37.9120850833, 175.4778879333, \"77\"],\n[-37.9072933667, 175.48012235, \"29\"],\n[-37.9058775167, 175.4812227667, \"20\"],\n[-37.9060436, 175.48114975, \"22\"],\n[-37.90620285, 175.4810768167, \"24\"],\n[-37.9063685833, 175.4809921833, \"26\"],\n[-37.9065447667, 175.4809131167, \"28\"],\n[-37.91495335, 175.47652165, \"111\"],\n[-37.9087348, 175.4794214667, \"41\"],\n[-37.9089801333, 175.47978555, \"42\"],\n[-37.9089057333, 175.4793457667, \"43\"],\n[-37.9095931667, 175.4790578, \"51\"],\n[-37.9123723167, 175.4777452667, \"81\"],\n[-37.9126095167, 175.47765015, \"91\"],\n[-37.91277265, 175.4775717667, \"93\"],\n[-37.91509955, 175.4768812333, \"110\"],\n[-37.9152698333, 175.476794, \"112\"],\n[-37.9154395, 175.4767326333, \"114\"],\n[-37.91572125, 175.4765971833, \"116\"],\n[-37.9102388667, 175.47919135, \"50\"],\n[-37.9048902, 175.4819941833, \"12A\"],\n[-37.9091995333, 175.47891055, \"47A\"],\n[-37.9150900667, 175.4764547333, \"113\"],\n[-37.91524275, 175.4763963833, \"115\"],\n[-37.9094314833, 175.4791159333, \"49\"],\n[-37.9096957667, 175.4794569, \"48A\"],\n[-37.9091736667, 175.4796928333, \"44\"],\n[-37.9104137167, 175.4791214333, \"52\"],\n[-37.9095526333, 175.47951535, \"48\"],\n[-37.9092440667, 175.4791909833, \"47\"],\n[-37.905025, 175.4816333, \"14\"],\n[-37.91218695, 175.4778348667, \"77A\"],\n[-37.9105796, 175.4790363667, \"54\"],\n[-37.9142164167, 175.4773281, \"102\"],\n[-37.9093673833, 175.4796006833, \"46\"],\n[-37.9107540667, 175.4789564333, \"56\"],\n[-37.9090764333, 175.4792701333, \"45\"],\n[-37.9122119833, 175.4774975, \"79\"],\n[-37.9051519, 175.4770494, \"5/37B\"],\n[-37.9072341833, 175.4812645, \"74\"],\n[-37.90539015, 175.47753615, \"21/37B\"],\n[-37.9064404833, 175.4786360667, \"52\"],\n[-37.9053830667, 175.4751337333, \"28\"],\n[-37.9053326, 175.4740749833, \"20A\"],\n[-37.9055134833, 175.4740488, \"20C\"],\n[-37.90547225, 175.47397175, \"20B\"],\n[-37.9053470167, 175.4742053667, \"20D\"],\n[-37.9069486333, 175.4814998167, \"73\"],\n[-37.9051562667, 175.4778512167, \"17/37B\"],\n[-37.9052251333, 175.4746236833, \"24\"],\n[-37.9053746, 175.4777257833, \"19/37B\"],\n[-37.9071479667, 175.4809996833, \"72\"],\n[-37.9050343167, 175.47739, \"24/37B\"],\n[-37.9049808, 175.4775142667, \"25/37B\"],\n[-37.9051211333, 175.4773504667, \"23/37B\"],\n[-37.9068252833, 175.4810905167, \"69\"],\n[-37.9047089333, 175.4739594, \"19\"],\n[-37.9040900833, 175.4718856167, \"1\"],\n[-37.9048747833, 175.4744885333, \"23\"],\n[-37.90479065, 175.4742263667, \"21\"],\n[-37.905018, 175.47392355, \"16\"],\n[-37.9063578667, 175.4783819333, \"50\"],\n[-37.9049504167, 175.4747584333, \"25\"],\n[-37.9050288833, 175.47502195, \"27\"],\n[-37.9072899333, 175.48271985, \"85\"],\n[-37.9054541333, 175.4776966667, \"20/37B\"],\n[-37.90488845, 175.4772389833, \"27/37B\"],\n[-37.9047877333, 175.4775819667, \"13/37B\"],\n[-37.9049796, 175.4771942833, \"28/37B\"],\n[-37.9073132333, 175.4815289167, \"76\"],\n[-37.9050827, 175.47699535, \"6/37B\"],\n[-37.9061134667, 175.4770216167, \"40A\"],\n[-37.90611365, 175.4768269667, \"38B\"],\n[-37.9076197, 175.4825873167, \"84\"],\n[-37.9070038333, 175.4817050667, \"75\"],\n[-37.9047039333, 175.47730165, \"11/37B\"],\n[-37.9075484833, 175.4823327833, \"82\"],\n[-37.9054264167, 175.4753033833, \"30\"],\n[-37.9069885833, 175.47905425, \"58\"],\n[-37.906228, 175.477926, \"48\"],\n[-37.9058200667, 175.4765282833, \"32\"],\n[-37.9046413667, 175.4737464333, \"17\"],\n[-37.9050075333, 175.4778885167, \"16/37B\"],\n[-37.90499485, 175.4768944167, \"7/37B\"],\n[-37.9046556167, 175.4771346, \"10/37B\"],\n[-37.90527195, 175.47690975, \"1/37B\"],\n[-37.9052758833, 175.4771711833, \"3/37B\"],\n[-37.9052169, 175.4771015167, \"4/37B\"],\n[-37.90529785, 175.4770102333, \"2/37B\"],\n[-37.9058489333, 175.47780495, \"45\"],\n[-37.9048925167, 175.4769351667, \"8/37B\"],\n[-37.9043826, 175.4728787667, \"15\"],\n[-37.9049060333, 175.47352905, \"12\"],\n[-37.9056042667, 175.47696875, \"37\"],\n[-37.9049538167, 175.47372635, \"14\"],\n[-37.9048069667, 175.4730782, \"10\"],\n[-37.90405505, 175.4727157833, \"11\"],\n[-37.9043442, 175.4726702333, \"13\"],\n[-37.9039020833, 175.4723256667, \"5\"],\n[-37.9046807667, 175.4726619167, \"6\"],\n[-37.9047340667, 175.47286755, \"8\"],\n[-37.9050737, 175.4741203667, \"18\"],\n[-37.90621865, 175.47643995, \"34\"],\n[-37.9055253667, 175.4767246667, \"35\"],\n[-37.9062645167, 175.4765613333, \"36\"],\n[-37.9059193, 175.4768274667, \"38\"],\n[-37.90570735, 175.4773252333, \"39\"],\n[-37.9059801833, 175.4770579, \"40\"],\n[-37.9062604167, 175.4772333833, \"42B\"],\n[-37.9057853667, 175.4775903, \"43\"],\n[-37.9061028167, 175.4774772667, \"44\"],\n[-37.90589465, 175.47796755, \"47\"],\n[-37.9068853333, 175.48130165, \"71\"],\n[-37.9071345833, 175.4821319833, \"79\"],\n[-37.9074696333, 175.4820778, \"80A\"],\n[-37.90765195, 175.4820632833, \"80B\"],\n[-37.9071948, 175.4823366167, \"81\"],\n[-37.90725725, 175.4825442, \"83\"],\n[-37.9042386667, 175.4723893667, \"7\"],\n[-37.9049427667, 175.47739025, \"26/37B\"],\n[-37.9048325667, 175.4777169, \"14/37B\"],\n[-37.9048868, 175.4778717167, \"15/37B\"],\n[-37.9052664833, 175.4777734333, \"18/37B\"],\n[-37.9053551, 175.4774051333, \"22/37B\"],\n[-37.90398685, 175.4726027167, \"9\"],\n[-37.9047089333, 175.4770192, \"9/37B\"],\n[-37.9061652333, 175.4777069667, \"46\"],\n[-37.9067892667, 175.4808744667, \"67\"],\n[-37.9068334667, 175.47996825, \"66\"],\n[-37.9067120833, 175.4795692, \"60\"],\n[-37.9041594, 175.4721355, \"3\"],\n[-37.9046192333, 175.4724616667, \"4\"],\n[-37.9050749, 175.47524375, \"29\"],\n[-37.9068698, 175.4786657833, \"54\"],\n[-37.9057202, 175.4762706, \"32B\"],\n[-37.9051451333, 175.4743827, \"22\"],\n[-37.90625565, 175.4769934167, \"40B\"],\n[-37.90604295, 175.4772728667, \"42A\"],\n[-37.9047528, 175.4774456167, \"12/37B\"],\n[-37.9073914, 175.4817937167, \"78\"],\n[-37.9070766667, 175.4819295333, \"77\"],\n[-37.8755341, 175.4678237, \"7\"],\n[-37.87529185, 175.467879, \"5\"],\n[-37.8751691333, 175.4682586333, \"6\"],\n[-37.8759656, 175.4679058667, \"13\"],\n[-37.87506995, 175.4678763667, \"3\"],\n[-37.8749979833, 175.46825515, \"4\"],\n[-37.8748353667, 175.4678732833, \"1\"],\n[-37.8755232, 175.4682126833, \"10\"],\n[-37.87481615, 175.4682566167, \"2\"],\n[-37.8759362, 175.4677101333, \"11\"],\n[-37.8756999, 175.4681576833, \"12\"],\n[-37.87587545, 175.4680599833, \"14\"],\n[-37.8753491, 175.4682464333, \"8\"],\n[-37.8757606333, 175.46762115, \"9\"],\n[-37.8187813333, 175.5099677833, \"62\"],\n[-37.81924235, 175.5087604667, \"49\"],\n[-37.8177396167, 175.5119011167, \"86\"],\n[-37.8178875667, 175.51156545, \"84\"],\n[-37.8174387167, 175.51263255, \"90\"],\n[-37.83408825, 175.55531685, \"1/1101\"],\n[-37.8175450333, 175.51885165, \"150\"],\n[-37.8077506333, 175.541635, \"400\"],\n[-37.8136809, 175.5290861, \"265\"],\n[-37.8110874, 175.53251965, \"304\"],\n[-37.8095875, 175.5753655167, \"2/729\"],\n[-37.83343, 175.5554701333, \"1101\"],\n[-37.8173373833, 175.5201625167, \"160\"],\n[-37.80918625, 175.5753678333, \"1/729\"],\n[-37.8101949167, 175.5340213667, \"320\"],\n[-37.8114688333, 175.5313781167, \"299\"],\n[-37.8110666833, 175.5317111833, \"301\"],\n[-37.8174875833, 175.5162427833, \"126\"],\n[-37.8121336833, 175.5303464167, \"289\"],\n[-37.8180013167, 175.5178742, \"2/138\"],\n[-37.81761695, 175.51742335, \"1/138\"],\n[-37.8074874667, 175.5440482, \"430\"],\n[-37.80752155, 175.5425832333, \"416\"],\n[-37.8366309333, 175.5496806333, \"1173\"],\n[-37.8169579833, 175.5214218667, \"179\"],\n[-37.8298305, 175.5634204, \"1007\"],\n[-37.8149217167, 175.5283630333, \"247\"],\n[-37.8217308333, 175.5663090333, \"910\"],\n[-37.81460935, 175.5745359167, \"797\"],\n[-37.8234203333, 175.5658778, \"924\"],\n[-37.8319167167, 175.5559498, \"1086\"],\n[-37.8306048167, 175.5616193833, \"1023\"],\n[-37.81758865, 175.57007665, \"851\"],\n[-37.8329407, 175.55562365, \"1099\"],\n[-37.81693675, 175.5123568167, \"95\"],\n[-37.8182193, 175.5131360833, \"88\"],\n[-37.8130308833, 175.5297032667, \"273\"],\n[-37.8095776, 175.55597085, \"544\"],\n[-37.8072376833, 175.5452215833, \"443\"],\n[-37.81966705, 175.5090003167, \"48\"],\n[-37.8169615333, 175.5198118833, \"159\"],\n[-37.8169314, 175.51421915, \"112\"],\n[-37.8080136167, 175.5463236, \"452\"],\n[-37.807907, 175.5458772167, \"448\"],\n[-37.80796455, 175.5461119167, \"450\"],\n[-37.8168847833, 175.5220721833, \"181\"],\n[-37.8354269, 175.5522720333, \"1145\"],\n[-37.83545425, 175.5543820167, \"1129\"],\n[-37.8082469167, 175.5385727167, \"376\"],\n[-37.8076848, 175.5419475167, \"408\"],\n[-37.8162252667, 175.5265092167, \"225\"],\n[-37.8166361333, 175.52144, \"1/179\"],\n[-37.8070565833, 175.5441373667, \"429\"],\n[-37.8075943333, 175.5444878167, \"432\"],\n[-37.8221778667, 175.5667572, \"911\"],\n[-37.8179158667, 175.5105938667, \"73\"],\n[-37.807556, 175.5460459333, \"447\"],\n[-37.8080632333, 175.5465634, \"454\"],\n[-37.8070305833, 175.54991865, \"483\"],\n[-37.80831265, 175.5514465667, \"526\"],\n[-37.80744605, 175.5634000833, \"603\"],\n[-37.8164026833, 175.5278071167, \"234\"],\n[-37.80656415, 175.5705962833, \"665\"],\n[-37.81898205, 175.5685284167, \"862\"],\n[-37.8236973167, 175.56648815, \"927\"],\n[-37.8256295833, 175.5653636167, \"965\"],\n[-37.8142507167, 175.57521965, \"789\"],\n[-37.8094723167, 175.5346592, \"349\"],\n[-37.8098434333, 175.5333155167, \"319\"],\n[-37.8292230167, 175.5635525167, \"1005\"],\n[-37.8173514167, 175.5221399167, \"180\"],\n[-37.83840865, 175.5469221167, \"1213\"],\n[-37.8379536333, 175.5475567833, \"1201\"],\n[-37.81720545, 175.5149902333, \"116\"],\n[-37.8085928833, 175.5380488, \"370\"],\n[-37.80779995, 175.5386938833, \"379\"],\n[-37.8204958333, 175.50876015, \"34\"],\n[-37.82623815, 175.56358455, \"970\"],\n[-37.81448725, 175.5734910333, \"804\"],\n[-37.8222833167, 175.5659173667, \"912\"],\n[-37.8170449333, 175.5277756167, \"1/232\"],\n[-37.8176035333, 175.5269654167, \"2/232\"],\n[-37.8323760333, 175.5575159333, \"1078\"],\n[-37.83021075, 175.5612354667, \"1024\"],\n[-37.9305489, 175.5017878333, \"90\"],\n[-37.93116365, 175.5055481833, \"122\"],\n[-37.9319767167, 175.5109385, \"168\"],\n[-37.93029815, 175.4998441167, \"72\"],\n[-37.9291897167, 175.4957679, \"35\"],\n[-37.92986625, 175.4972463167, \"50\"],\n[-37.9290868833, 175.4948005167, \"25\"],\n[-37.9296040667, 175.4987881667, \"63\"],\n[-37.9309512167, 175.5043885167, \"110\"],\n[-37.9296868833, 175.4958722667, \"38\"],\n[-37.9295072167, 175.4977275833, \"51\"],\n[-37.9303624833, 175.5006042333, \"74\"],\n[-37.9319759, 175.5106025, \"166\"],\n[-37.9306431833, 175.5024661, \"96\"],\n[-37.9299079167, 175.4976426667, \"52\"],\n[-37.9312659333, 175.5064569833, \"132\"],\n[-37.93082875, 175.50372585, \"106\"],\n[-37.9320732833, 175.5108069, \"167\"],\n[-37.8443465167, 175.36530835, \"32\"],\n[-37.8448259333, 175.3650265167, \"36\"],\n[-37.8425576167, 175.3664466333, \"8\"],\n[-37.8925954833, 175.45710755, \"3\"],\n[-37.8930169167, 175.45739025, \"13\"],\n[-37.89310595, 175.4574966167, \"17\"],\n[-37.8931706667, 175.4571386, \"19\"],\n[-37.8930694833, 175.45792925, \"15\"],\n[-37.892484, 175.4570883333, \"1\"],\n[-37.8926542167, 175.4574440667, \"5\"],\n[-37.89269795, 175.4575341667, \"7\"],\n[-37.8926711333, 175.4579661, \"9\"],\n[-37.8929884833, 175.4571287, \"11\"],\n[-37.883121, 175.4810597, \"11\"],\n[-37.8827995833, 175.4808762167, \"15\"],\n[-37.8827993, 175.4810446833, \"15A\"],\n[-37.88305445, 175.4806939167, \"14\"],\n[-37.8832812833, 175.4806736333, \"12\"],\n[-37.8829723, 175.4809516, \"13\"],\n[-37.8836668, 175.4806524333, \"8\"],\n[-37.88350115, 175.4806812167, \"10\"],\n[-37.8838327667, 175.4806120333, \"6\"],\n[-37.8836368, 175.4810848667, \"5\"],\n[-37.8840162333, 175.4806070667, \"4\"],\n[-37.8841447, 175.4806001167, \"2\"],\n[-37.8837898, 175.4810769333, \"3\"],\n[-37.8834789167, 175.4810792167, \"7\"],\n[-37.8832999833, 175.4810661333, \"9\"],\n[-37.8839425333, 175.4810822667, \"1\"],\n[-37.8741644333, 175.466663, \"1\"],\n[-37.8741701833, 175.4671011667, \"2\"],\n[-37.8734758833, 175.4661548667, \"11B\"],\n[-37.8730602167, 175.4660412333, \"17B\"],\n[-37.8732532833, 175.4669407833, \"12\"],\n[-37.8736860667, 175.4670495333, \"8\"],\n[-37.8738914, 175.4662481167, \"5B\"],\n[-37.8740275667, 175.46708325, \"4\"],\n[-37.8728965, 175.4668399167, \"16\"],\n[-37.87401705, 175.4666682333, \"3\"],\n[-37.8740028333, 175.4662564, \"5A\"],\n[-37.8734885167, 175.467, \"10\"],\n[-37.8738630833, 175.4670664333, \"6\"],\n[-37.8737826, 175.4666489667, \"7\"],\n[-37.8735874, 175.46617485, \"11A\"],\n[-37.8736024167, 175.4666129, \"9\"],\n[-37.8730790167, 175.4668945167, \"14\"],\n[-37.8729272167, 175.4664146833, \"19\"],\n[-37.8733468333, 175.4665393167, \"13\"],\n[-37.8731517, 175.4664815167, \"15\"],\n[-37.8731441167, 175.4660683, \"17A\"],\n[-37.8825944667, 175.4798169833, \"2\"],\n[-37.8826055167, 175.4805495667, \"10\"],\n[-37.8825809333, 175.4800131333, \"4\"],\n[-37.8825441333, 175.4804278333, \"8\"],\n[-37.8828469, 175.4800110667, \"3\"],\n[-37.882842, 175.4802305333, \"5\"],\n[-37.8825584833, 175.4801972667, \"6\"],\n[-37.8828376, 175.4804269167, \"7\"],\n[-37.88275275, 175.4805278667, \"9\"],\n[-37.8828507833, 175.4798377667, \"1\"],\n[-37.8747543667, 175.3655736, \"277\"],\n[-37.8779646667, 175.3771985, \"389\"],\n[-37.8791017167, 175.3701385167, \"1/326\"],\n[-37.8809893667, 175.38422785, \"458\"],\n[-37.8801277667, 175.3840239333, \"459\"],\n[-37.8814240333, 175.3849688833, \"468\"],\n[-37.8806415333, 175.38358955, \"452\"],\n[-37.8785101, 175.3802812167, \"409\"],\n[-37.8796759167, 175.3830951167, \"445\"],\n[-37.8803115167, 175.3830208833, \"446\"],\n[-37.8778165333, 175.3759684833, \"373\"],\n[-37.88137445, 175.3862355333, \"473\"],\n[-37.8782752833, 175.3789725, \"397\"],\n[-37.8808287667, 175.3851571, \"469\"],\n[-37.87896095, 175.3695827167, \"326\"],\n[-37.8765604833, 175.3662541167, \"288\"],\n[-37.8818931667, 175.38581595, \"474\"],\n[-37.8775986, 175.3747133333, \"2/359\"],\n[-37.8769582833, 175.3713375, \"331\"],\n[-37.8843103667, 175.3914426833, \"531\"],\n[-37.87595675, 175.3649699833, \"271\"],\n[-37.8781325, 175.3739238833, \"356\"],\n[-37.8763472667, 175.36476045, \"270\"],\n[-37.87676475, 175.3676648667, \"298\"],\n[-37.8769505833, 175.36918955, \"310\"],\n[-37.8766704667, 175.36975, \"313\"],\n[-37.8777166, 175.3720850333, \"334\"],\n[-37.8773668333, 175.3733521333, \"349\"],\n[-37.8772966667, 175.3744940833, \"1/359\"],\n[-37.87831085, 175.3791264167, \"399\"],\n[-37.8778886833, 175.37662545, \"377\"],\n[-37.8784267667, 175.3770323833, \"388\"],\n[-37.8790059333, 175.3798270667, \"408\"],\n[-37.91994325, 175.4698456167, \"5\"],\n[-37.9200047, 175.4693797833, \"6\"],\n[-37.9195710333, 175.4694752833, \"2\"],\n[-37.9203742667, 175.4697562833, \"7\"],\n[-37.9196343, 175.4699295667, \"3\"],\n[-37.9198162, 175.4690209, \"4\"],\n[-37.8824037833, 175.4600990333, \"5\"],\n[-37.88208185, 175.4598317167, \"9\"],\n[-37.8826121667, 175.4600836333, \"3\"],\n[-37.8826033333, 175.45961605, \"2\"],\n[-37.8822431167, 175.4600865, \"7\"],\n[-37.8821465833, 175.4595746, \"8\"],\n[-37.8823125667, 175.45958745, \"6\"],\n[-37.8824702667, 175.4595934167, \"4\"],\n[-37.9092579167, 175.46735845, \"6A\"],\n[-37.9089322167, 175.4674924, \"10\"],\n[-37.90894585, 175.4672735167, \"12\"],\n[-37.9089078333, 175.46711725, \"11\"],\n[-37.9089631667, 175.4669939667, \"9\"],\n[-37.90904455, 175.4673255, \"8\"],\n[-37.9094559, 175.4668010167, \"1\"],\n[-37.9093255, 175.4673469667, \"4A\"],\n[-37.9093457833, 175.46717355, \"4\"],\n[-37.9095302167, 175.4670570667, \"2\"],\n[-37.9090935, 175.4664936667, \"3\"],\n[-37.90917125, 175.4672602333, \"6\"],\n[-37.90908325, 175.4669295667, \"7\"],\n[-37.9092908333, 175.4668895333, \"5\"],\n[-37.8593663833, 175.4539760167, \"251\"],\n[-37.8754824, 175.4604578167, \"63\"],\n[-37.8750289833, 175.4602931333, \"67\"],\n[-37.7985084167, 175.4478579333, \"941\"],\n[-37.842417, 175.4531796833, \"443\"],\n[-37.8085080333, 175.45099665, \"810\"],\n[-37.79048555, 175.44243815, \"1063\"],\n[-37.8492731667, 175.4532494833, \"355\"],\n[-37.8094198833, 175.4503523667, \"819\"],\n[-37.8728692167, 175.4594153667, \"91\"],\n[-37.8522977333, 175.4539347833, \"344\"],\n[-37.8484041333, 175.45409895, \"372\"],\n[-37.8479726833, 175.45304965, \"387\"],\n[-37.7948086, 175.4476616833, \"994\"],\n[-37.7914089, 175.4452872333, \"1027\"],\n[-37.8080105, 175.4498779333, \"835\"],\n[-37.8722349333, 175.4592175167, \"99\"],\n[-37.8091485667, 175.45085495, \"821\"],\n[-37.8594542833, 175.4549678833, \"5/246\"],\n[-37.8230668667, 175.4541309833, \"660\"],\n[-37.8316753333, 175.4539613167, \"562\"],\n[-37.8387429833, 175.4531928, \"475\"],\n[-37.8545740667, 175.4540875333, \"294\"],\n[-37.7900112167, 175.4442665, \"1045\"],\n[-37.8172833167, 175.4539773167, \"724\"],\n[-37.83503155, 175.453934, \"542\"],\n[-37.8607128667, 175.4548002, \"2/241\"],\n[-37.8380142833, 175.45391425, \"490\"],\n[-37.8275037167, 175.4529093833, \"589\"],\n[-37.8596196, 175.4542151, \"249\"],\n[-37.8595530167, 175.45479165, \"3/246\"],\n[-37.85959605, 175.45471705, \"2/246\"],\n[-37.8592503167, 175.4544837333, \"252\"],\n[-37.8713457667, 175.4587725833, \"111\"],\n[-37.8595053, 175.4548791833, \"4/246\"],\n[-37.79668195, 175.4482608333, \"958\"],\n[-37.8227231, 175.4540730167, \"670\"],\n[-37.8750904833, 175.4593735333, \"71\"],\n[-37.8190378833, 175.4540990167, \"702\"],\n[-37.86957775, 175.4580359, \"129\"],\n[-37.8662393833, 175.4567130667, \"169\"],\n[-37.8687158167, 175.4577150333, \"141\"],\n[-37.86791495, 175.4573792167, \"151\"],\n[-37.8089932167, 175.4507184167, \"823\"],\n[-37.8666422833, 175.4569389667, \"167\"],\n[-37.8738722833, 175.4598649833, \"81\"],\n[-37.8169442833, 175.4534629333, \"727\"],\n[-37.8185275, 175.4534621333, \"709\"],\n[-37.8059395833, 175.44899615, \"859\"],\n[-37.8327434833, 175.4540100333, \"550\"],\n[-37.8604312833, 175.4546856333, \"1/241\"],\n[-37.8610548833, 175.4549434167, \"3/241\"],\n[-37.8596720667, 175.45475175, \"244\"],\n[-37.8603010167, 175.4551234333, \"238\"],\n[-37.874473, 175.4600767667, \"75\"],\n[-37.8192958833, 175.4546863, \"698\"],\n[-37.80070935, 175.4483520333, \"917\"],\n[-37.8595345, 175.4546793167, \"250\"],\n[-37.8395576167, 175.4538942167, \"470\"],\n[-37.8600606333, 175.4551961167, \"242\"],\n[-37.8587876, 175.45357305, \"261\"],\n[-37.8202505167, 175.454168, \"692\"],\n[-37.83062305, 175.4541314, \"574\"],\n[-37.85965015, 175.4546251667, \"1/246\"],\n[-37.87094345, 175.4585491667, \"115\"],\n[-37.82367025, 175.4541148167, \"654\"],\n[-37.8761585333, 175.4607254667, \"55\"],\n[-37.8650849833, 175.4562546333, \"183\"],\n[-37.8104551333, 175.45176685, \"803\"],\n[-37.8601735167, 175.4545659833, \"247\"],\n[-37.8072936667, 175.4501624667, \"842\"],\n[-37.81121545, 175.4523795667, \"795\"],\n[-37.8082023667, 175.4500136333, \"833\"],\n[-37.8088097833, 175.4505667167, \"825\"],\n[-37.8073512333, 175.4494723833, \"841\"],\n[-37.8180043167, 175.45347675, \"713\"],\n[-37.8034810667, 175.44930745, \"884A\"],\n[-37.8133904167, 175.4533446667, \"767\"],\n[-37.7906012833, 175.4456008, \"1032\"],\n[-37.8036063333, 175.44934055, \"884\"],\n[-37.8086218, 175.4503990167, \"827\"],\n[-37.7990523833, 175.4480464, \"931\"],\n[-37.8435690667, 175.40554535, \"296A\"],\n[-37.84511005, 175.4057523167, \"282\"],\n[-37.8456481333, 175.4283544, \"93\"],\n[-37.8452106, 175.4213902167, \"154\"],\n[-37.8456685667, 175.4217703833, \"151\"],\n[-37.8450910333, 175.4066185167, \"296B\"],\n[-37.84556515, 175.3859519, \"471\"],\n[-37.8456769, 175.4356176167, \"29\"],\n[-37.8452146333, 175.4326596167, \"54\"],\n[-37.8456379667, 175.4318747833, \"59\"],\n[-37.8451947667, 175.4291500167, \"82\"],\n[-37.8456449667, 175.42712405, \"103\"],\n[-37.8452215333, 175.4202044833, \"166\"],\n[-37.84562355, 175.4192034833, \"173\"],\n[-37.84520295, 175.4171995667, \"190\"],\n[-37.8451047667, 175.41595175, \"204\"],\n[-37.84557985, 175.4160677, \"201\"],\n[-37.8451894667, 175.4139933167, \"218\"],\n[-37.8455275833, 175.4075856833, \"277\"],\n[-37.8455757667, 175.40668145, \"281\"],\n[-37.8451309667, 175.4038956667, \"308\"],\n[-37.8455923333, 175.40336095, \"313\"],\n[-37.8451457333, 175.4006193, \"332\"],\n[-37.8456046667, 175.3996052667, \"345\"],\n[-37.8450766, 175.3979973833, \"362A\"],\n[-37.8450326167, 175.3961487833, \"376\"],\n[-37.84556585, 175.3949421167, \"389\"],\n[-37.84503405, 175.3928639333, \"406\"],\n[-37.8450819, 175.3938952833, \"398\"],\n[-37.8449702833, 175.39000315, \"430\"],\n[-37.8455388, 175.3929698833, \"403\"],\n[-37.8455067667, 175.389923, \"431\"],\n[-37.8449729167, 175.38231105, \"498\"],\n[-37.8449880833, 175.38162075, \"502\"],\n[-37.8432495, 175.3824779667, \"496\"],\n[-37.8441658167, 175.4248163667, \"122\"],\n[-37.84258355, 175.3809411333, \"512\"],\n[-37.8451205833, 175.41321575, \"228\"],\n[-37.8454313833, 175.3813477333, \"509\"],\n[-37.8449337333, 175.3830211333, \"494\"],\n[-37.8456650333, 175.4344218333, \"1/41\"],\n[-37.8450306667, 175.3974560667, \"362C\"],\n[-37.8463207667, 175.4343822167, \"2/42\"],\n[-37.8450416833, 175.3967473667, \"372\"],\n[-37.8455358167, 175.4085091, \"267\"],\n[-37.8451470333, 175.4076077167, \"276\"],\n[-37.84420195, 175.39791495, \"362B\"],\n[-37.8452185667, 175.4259005667, \"114\"],\n[-37.8456212833, 175.4347298667, \"39\"],\n[-37.8452003167, 175.40904765, \"262\"],\n[-37.8451996667, 175.4122621833, \"234\"],\n[-37.84515415, 175.4098542167, \"256\"],\n[-37.8455715833, 175.4139006833, \"223\"],\n[-37.8455915333, 175.4136348333, \"225\"],\n[-37.8455279667, 175.3870696, \"463\"],\n[-37.8457080167, 175.4225169667, \"145\"],\n[-37.8450449333, 175.3784177833, \"536\"],\n[-37.8441582, 175.3843482, \"480\"],\n[-37.8441567667, 175.38280735, \"494A\"],\n[-37.8450534333, 175.3804865833, \"516\"],\n[-37.845458, 175.3819993833, \"503\"],\n[-37.8416720667, 175.4161419167, \"202\"],\n[-37.84561615, 175.4169546167, \"193\"],\n[-37.8456090833, 175.4248031, \"127\"],\n[-37.8452347167, 175.4210866, \"156\"],\n[-37.8452366333, 175.4236209667, \"136\"],\n[-37.8420864167, 175.3808174, \"514\"],\n[-37.9352309167, 175.4932222, \"138\"],\n[-37.9344554333, 175.48793725, \"92\"],\n[-37.93352515, 175.48479765, \"59\"],\n[-37.9333442833, 175.4806992167, \"30\"],\n[-37.9333265, 175.4832372833, \"43\"],\n[-37.9338463667, 175.4867482, \"81\"],\n[-37.9349315333, 175.4913314333, \"109\"],\n[-37.9335968833, 175.4853604, \"63\"],\n[-37.9316883, 175.48456045, \"53\"],\n[-37.9338528333, 175.4839506, \"52\"],\n[-37.8310454167, 175.5756857833, \"775\"],\n[-37.8312273667, 175.5775630667, \"757\"],\n[-37.8293654833, 175.5685721667, \"851\"],\n[-37.8285495833, 175.56486995, \"881\"],\n[-37.8309590333, 175.5733609, \"801\"],\n[-37.8305305333, 175.5736851833, \"790\"],\n[-37.8295682333, 175.56931885, \"841\"],\n[-37.83130275, 175.5782757167, \"753\"],\n[-37.8283176167, 175.5655312333, \"879\"],\n[-37.8540068167, 175.37670555, \"34\"],\n[-37.85430545, 175.3768498167, \"32\"],\n[-37.8549918167, 175.3798488333, \"5\"],\n[-37.85449605, 175.37689005, \"33\"],\n[-37.8547125333, 175.3802214167, \"4\"],\n[-37.8545867833, 175.3781517833, \"28\"],\n[-37.8547440667, 175.37743325, \"31\"],\n[-37.85420255, 175.3773112333, \"30\"],\n[-37.8879053333, 175.5233262333, \"1/18\"],\n[-37.8881771333, 175.5232950167, \"2/18\"],\n[-37.8834455333, 175.45836535, \"92\"],\n[-37.8863947167, 175.4586610667, \"58B\"],\n[-37.8866006667, 175.45961975, \"55A\"],\n[-37.8867239167, 175.4592584167, \"55\"],\n[-37.88667, 175.4586812, \"56\"],\n[-37.8817044333, 175.4588333167, \"113\"],\n[-37.8815483333, 175.4587898667, \"115\"],\n[-37.8814229167, 175.4587740833, \"117\"],\n[-37.8913856667, 175.4590918167, \"18\"],\n[-37.8915707667, 175.4591145167, \"16\"],\n[-37.8868740833, 175.4599066167, \"53C\"],\n[-37.88889255, 175.4599269833, \"37B\"],\n[-37.89262475, 175.4600617167, \"7A\"],\n[-37.8889847167, 175.4594067667, \"37A\"],\n[-37.8925890833, 175.4585589, \"10B\"],\n[-37.8824604, 175.4588691333, \"105\"],\n[-37.8836036333, 175.4583701, \"90\"],\n[-37.8908670667, 175.4585197333, \"24A\"],\n[-37.8826975167, 175.4583391, \"98\"],\n[-37.8835235167, 175.4590010333, \"95\"],\n[-37.8838230333, 175.4583829333, \"88\"],\n[-37.8845143667, 175.4579167, \"80A\"],\n[-37.8828394333, 175.4583557667, \"96\"],\n[-37.88251835, 175.4583208667, \"100\"],\n[-37.8926768167, 175.4583797667, \"10C\"],\n[-37.88716595, 175.4586927333, \"50\"],\n[-37.8883195667, 175.4593362333, \"43\"],\n[-37.8864967833, 175.4592373833, \"59\"],\n[-37.8871386833, 175.4592945833, \"51\"],\n[-37.8836205667, 175.4589701333, \"91\"],\n[-37.8870057667, 175.4587382167, \"52\"],\n[-37.8823566667, 175.4583266833, \"102\"],\n[-37.8867508, 175.45831015, \"2/54\"],\n[-37.88680585, 175.4583326167, \"54B\"],\n[-37.8869064, 175.4592745833, \"53\"],\n[-37.8836770667, 175.45788765, \"90A\"],\n[-37.88377685, 175.45896945, \"91A\"],\n[-37.8844059167, 175.4593669667, \"83A\"],\n[-37.8843895833, 175.4590283, \"83\"],\n[-37.88413765, 175.4580580833, \"84A\"],\n[-37.8909576167, 175.4590556833, \"22\"],\n[-37.8932342, 175.4592408833, \"4\"],\n[-37.8929272167, 175.45863745, \"8B\"],\n[-37.8929735167, 175.4583677333, \"8C\"],\n[-37.8859658167, 175.4592039, \"71\"],\n[-37.8881317333, 175.4593295333, \"45\"],\n[-37.88894835, 175.45890335, \"36\"],\n[-37.88873325, 175.4588844167, \"38\"],\n[-37.8885017167, 175.45935385, \"41A\"],\n[-37.8888022667, 175.4593873, \"39\"],\n[-37.88814385, 175.4586047333, \"44A\"],\n[-37.8854610167, 175.4585890667, \"66\"],\n[-37.89301925, 175.4597807333, \"3\"],\n[-37.8888152167, 175.4597877833, \"37C\"],\n[-37.88442445, 175.4579215333, \"82A\"],\n[-37.8841846833, 175.4584516, \"84\"],\n[-37.8858883833, 175.4596244167, \"71A\"],\n[-37.8911121167, 175.4596169167, \"21\"],\n[-37.8882564333, 175.4588584167, \"42\"],\n[-37.8839837, 175.4584101, \"86\"],\n[-37.8862705, 175.4595605667, \"63\"],\n[-37.8878208, 175.4588156833, \"46\"],\n[-37.8891674833, 175.4594178, \"35\"],\n[-37.8884817, 175.4588827667, \"40\"],\n[-37.8845497667, 175.4584736333, \"80\"],\n[-37.8908341667, 175.45958975, \"23\"],\n[-37.8926482, 175.4588475667, \"10A\"],\n[-37.8846152, 175.4590487333, \"81\"],\n[-37.8868875833, 175.4597079333, \"53B\"],\n[-37.8867661667, 175.4580206, \"1/54\"],\n[-37.8923631167, 175.4601005833, \"11A\"],\n[-37.8880116333, 175.4588415167, \"44\"],\n[-37.88179955, 175.4582856667, \"108\"],\n[-37.8819699667, 175.4588568833, \"109\"],\n[-37.8906476667, 175.4590157667, \"28\"],\n[-37.8846581333, 175.4584694833, \"78\"],\n[-37.8843345167, 175.4584402833, \"82\"],\n[-37.8841375167, 175.4590033, \"85\"],\n[-37.88394965, 175.4578898667, \"86A\"],\n[-37.88396905, 175.45758895, \"86B\"],\n[-37.8837848667, 175.4578930833, \"88A\"],\n[-37.8839426667, 175.4589834333, \"89\"],\n[-37.8923547333, 175.4596697667, \"11\"],\n[-37.8916483333, 175.45965705, \"13\"],\n[-37.89177585, 175.4591300667, \"14\"],\n[-37.8914522167, 175.459646, \"15\"],\n[-37.8912940833, 175.4596277667, \"17\"],\n[-37.8923723833, 175.4592214, \"12\"],\n[-37.8907689667, 175.4590306667, \"24\"],\n[-37.8904437833, 175.4595327, \"25A\"],\n[-37.8906071167, 175.4595718167, \"25\"],\n[-37.8902158167, 175.4595238167, \"27\"],\n[-37.8911627, 175.4590753167, \"20\"],\n[-37.8886534333, 175.4593824833, \"41\"],\n[-37.8930344167, 175.4600257833, \"3A-3F\"],\n[-37.8865167167, 175.4586738833, \"58\"],\n[-37.8863451667, 175.4592180667, \"61\"],\n[-37.88626165, 175.45980935, \"65\"],\n[-37.8862540167, 175.4586216, \"60\"],\n[-37.8858030167, 175.4586246, \"62\"],\n[-37.8856375333, 175.4586118667, \"64\"],\n[-37.89284255, 175.4600622333, \"5A\"],\n[-37.8928040667, 175.4597492333, \"5\"],\n[-37.8861293, 175.4592111833, \"69\"],\n[-37.8858680333, 175.4598526167, \"71B\"],\n[-37.885777, 175.4599270167, \"71C\"],\n[-37.8857422667, 175.4597478167, \"71D\"],\n[-37.88574365, 175.45918075, \"73\"],\n[-37.8855746833, 175.4591693833, \"75\"],\n[-37.8930622167, 175.4592157833, \"6\"],\n[-37.8925581, 175.4603171, \"7B\"],\n[-37.8926518167, 175.46025555, \"7C\"],\n[-37.8929233, 175.4588164167, \"8A\"],\n[-37.8928556667, 175.4592156833, \"8\"],\n[-37.8924033, 175.4602587333, \"9\"],\n[-37.8826857833, 175.4589041167, \"103\"],\n[-37.8868134333, 175.4586863667, \"54\"],\n[-37.8821768333, 175.4588641833, \"107\"],\n[-37.8925898333, 175.4596910333, \"7\"],\n[-37.8862095667, 175.46007345, \"67\"],\n[-37.8893183667, 175.4594386167, \"33\"],\n[-37.88182795, 175.4588519167, \"111\"],\n[-37.8853995, 175.4591325167, \"77\"],\n[-37.8819874833, 175.4582899, \"106\"],\n[-37.8868931667, 175.4594645667, \"53A\"],\n[-37.89067275, 175.4587377333, \"28A\"],\n[-37.88729905, 175.45870125, \"48\"],\n[-37.8872955667, 175.4593124, \"49\"],\n[-37.8926355167, 175.4591491667, \"10\"],\n[-37.8821580833, 175.45830775, \"104\"],\n[-37.8581124833, 175.3783852167, \"535\"],\n[-37.8652348667, 175.3746078, \"454\"],\n[-37.8528962667, 175.3764204333, \"591C\"],\n[-37.8658603333, 175.3777156333, \"466B\"],\n[-37.8561505333, 175.3796002833, \"563\"],\n[-37.8574877833, 175.3782657333, \"557\"],\n[-37.8648613667, 175.3759225833, \"466A\"],\n[-37.8680267, 175.3758790667, \"422\"],\n[-37.8679018667, 175.3731075, \"420\"],\n[-37.85081775, 175.3829343167, \"643\"],\n[-37.8671912, 175.36468055, \"352\"],\n[-37.86727615, 175.36543555, \"360\"],\n[-37.8501304, 175.3806456, \"637\"],\n[-37.8673357333, 175.3661331333, \"366A\"],\n[-37.8673979333, 175.3667809, \"366B\"],\n[-37.8526604333, 175.3775103333, \"591B\"],\n[-37.8590671, 175.3777331, \"525\"],\n[-37.8604441333, 175.37691195, \"511A\"],\n[-37.8674681167, 175.3678874333, \"374\"],\n[-37.8677148833, 175.3698444833, \"402\"],\n[-37.8666863, 175.3663322333, \"365\"],\n[-37.8675554, 175.3754905167, \"424A\"],\n[-37.8676465167, 175.37623815, \"424\"],\n[-37.86661535, 175.37609495, \"442\"],\n[-37.8598401833, 175.3771549167, \"511B\"],\n[-37.8661314333, 175.3647742667, \"353\"],\n[-37.8666274333, 175.3736697167, \"434\"],\n[-37.8662379333, 175.3738677667, \"438\"],\n[-37.8520614, 175.3823100833, \"613\"],\n[-37.8521965167, 175.3812996167, \"607\"],\n[-37.85242245, 175.3821009333, \"605\"],\n[-37.8520654833, 175.3761835667, \"591A\"],\n[-37.8547938667, 175.3751005167, \"591D\"],\n[-37.8600232, 175.3780412167, \"516\"],\n[-37.8504624833, 175.3807091333, \"635B\"],\n[-37.86694425, 175.3698350167, \"387\"],\n[-37.8512808, 175.3826955167, \"635A\"],\n[-37.8626598333, 175.3763083833, \"480\"],\n[-37.8631832667, 175.3759811, \"478\"],\n[-37.8597738, 175.3758612333, \"511\"],\n[-37.8669354833, 175.3734885833, \"432\"],\n[-37.8660254333, 175.3741769333, \"440\"],\n[-37.8677621333, 175.3708737667, \"418\"],\n[-37.86470325, 175.3749985333, \"456\"],\n[-37.8496086167, 175.3843649167, \"660\"],\n[-37.8533389167, 175.3814388333, \"599\"],\n[-37.8513108, 175.3833057167, \"636\"],\n[-37.8582031333, 175.3789229667, \"536\"],\n[-37.8049091167, 175.4582433833, \"111\"],\n[-37.79217, 175.4610993667, \"235\"],\n[-37.7912009, 175.46131515, \"243\"],\n[-37.8073855167, 175.4519374833, \"17\"],\n[-37.7901330667, 175.4618390333, \"259\"],\n[-37.7979543, 175.46169615, \"186\"],\n[-37.7953406667, 175.4605087333, \"199\"],\n[-37.8075016167, 175.4517328833, \"15\"],\n[-37.7944439333, 175.4604859167, \"221\"],\n[-37.8960575333, 175.47501025, \"14\"],\n[-37.8979342, 175.4745448, \"31A\"],\n[-37.8968049167, 175.47448785, \"24A\"],\n[-37.8974204333, 175.475026, \"25\"],\n[-37.8977612, 175.47473925, \"29\"],\n[-37.89678435, 175.4748293, \"20\"],\n[-37.89589205, 175.4749720833, \"12\"],\n[-37.8966669, 175.4743795667, \"22\"],\n[-37.8958911333, 175.4761947, \"5\"],\n[-37.8971542833, 175.4752421167, \"21\"],\n[-37.8960713, 175.4760678, \"7\"],\n[-37.8971206833, 175.47398845, \"30\"],\n[-37.8959413333, 175.4751180667, \"10A\"],\n[-37.89774365, 175.4741330667, \"40\"],\n[-37.8964759167, 175.4757599833, \"13\"],\n[-37.8956967, 175.4756972333, \"6\"],\n[-37.8969728167, 175.4746339333, \"24\"],\n[-37.8975985667, 175.4742274833, \"38\"],\n[-37.8964206167, 175.4764929, \"9\"],\n[-37.89591475, 175.4748164, \"14A\"],\n[-37.8963322333, 175.4747763333, \"18A\"],\n[-37.8962043333, 175.4746025333, \"18B\"],\n[-37.8969914833, 175.47434705, \"26A\"],\n[-37.8975744667, 175.4748792167, \"27\"],\n[-37.8979075667, 175.4740015333, \"42\"],\n[-37.89599185, 175.4754597167, \"10\"],\n[-37.89628205, 175.4758701, \"11\"],\n[-37.8966986167, 175.4755950667, \"15\"],\n[-37.8954172667, 175.4758408833, \"2\"],\n[-37.8955642833, 175.47581195, \"4\"],\n[-37.8957927167, 175.4756119833, \"8\"],\n[-37.8974439333, 175.4743467667, \"36\"],\n[-37.8954518167, 175.4753103, \"6A\"],\n[-37.89718045, 175.4745253167, \"26\"],\n[-37.89567315, 175.4763134667, \"1\"],\n[-37.8957657667, 175.4763043333, \"3\"],\n[-37.8965164667, 175.4750518, \"18\"],\n[-37.89807905, 175.47440185, \"31\"],\n[-37.8968958, 175.4754382833, \"17\"],\n[-37.89631735, 175.4752079667, \"16\"],\n[-37.8972125833, 175.4741699833, \"34\"],\n[-37.8970015667, 175.47401225, \"28\"],\n[-37.9133573833, 175.4717484667, \"287\"],\n[-37.9048133167, 175.47635, \"104\"],\n[-37.9049925, 175.4762745167, \"106\"],\n[-37.90467735, 175.4753654167, \"107\"],\n[-37.90048465, 175.4783948167, \"58\"],\n[-37.8992426833, 175.4793293167, \"42\"],\n[-37.9046434667, 175.4764180667, \"102\"],\n[-37.91292555, 175.47197745, \"285\"],\n[-37.9061096167, 175.4751589833, \"127\"],\n[-37.9059487333, 175.4760527167, \"120B\"],\n[-37.9058349, 175.4758649667, \"120A\"],\n[-37.9046025333, 175.47507605, \"107A\"],\n[-37.9043145167, 175.4765927167, \"98\"],\n[-37.9026473833, 175.4773607333, \"80\"],\n[-37.8985112833, 175.4791536, \"37\"],\n[-37.90029415, 175.4779031333, \"57\"],\n[-37.9157889333, 175.4706511667, \"307\"],\n[-37.9161504167, 175.4698894, \"317\"],\n[-37.9162483333, 175.47040295, \"315\"],\n[-37.9119694833, 175.4724158167, \"273\"],\n[-37.9127305, 175.4727451167, \"280A\"],\n[-37.910892, 175.4729321833, \"257\"],\n[-37.91104955, 175.4723925167, \"261\"],\n[-37.91155295, 175.4731748833, \"260\"],\n[-37.9104050667, 175.4728724167, \"249\"],\n[-37.91055955, 175.4730871, \"251\"],\n[-37.9074686167, 175.4751091833, \"188\"],\n[-37.9079151, 175.4751543167, \"192A\"],\n[-37.9060373, 175.4757691167, \"136\"],\n[-37.90429105, 175.4760343833, \"97\"],\n[-37.9123212333, 175.4728413167, \"276\"],\n[-37.91122575, 175.4733142333, \"240\"],\n[-37.9028192, 175.4767472833, \"83\"],\n[-37.8979333667, 175.4794421167, \"31\"],\n[-37.8977516, 175.4801524833, \"28\"],\n[-37.89934065, 175.4785610667, \"47\"],\n[-37.9103580667, 175.4731422667, \"247\"],\n[-37.9106933167, 175.4735704667, \"238\"],\n[-37.90118205, 175.4774954833, \"65\"],\n[-37.9051274833, 175.4765782667, \"106A\"],\n[-37.9001666333, 175.4786251833, \"54\"],\n[-37.8996917833, 175.4776611333, \"53A\"],\n[-37.9157449833, 175.4701549667, \"311\"],\n[-37.8998454833, 175.47809685, \"53\"],\n[-37.8999531167, 175.4780619, \"55\"],\n[-37.9123259333, 175.4731539833, \"274\"],\n[-37.91203635, 175.4729645833, \"262\"],\n[-37.9025673667, 175.4766802, \"79A\"],\n[-37.8995076667, 175.4784470167, \"49\"],\n[-37.9160592167, 175.4710586333, \"316\"],\n[-37.9045513333, 175.4752828833, \"105\"],\n[-37.901886, 175.4778055167, \"74B\"],\n[-37.9007665833, 175.4782302, \"60\"],\n[-37.9027685, 175.4767654333, \"81\"],\n[-37.9042667667, 175.47545395, \"99\"],\n[-37.90210725, 175.4776131, \"76\"],\n[-37.914356, 175.4712761333, \"299\"],\n[-37.9037502167, 175.4768480333, \"94\"],\n[-37.8975508667, 175.48021795, \"22\"],\n[-37.9126604333, 175.4720775, \"281\"],\n[-37.90140745, 175.4783687833, \"66\"],\n[-37.9008916333, 175.4774276833, \"63\"],\n[-37.91678785, 175.47071965, \"322\"],\n[-37.9088462667, 175.4738790833, \"227\"],\n[-37.9123182167, 175.4722337, \"277\"],\n[-37.90000635, 175.47873665, \"52\"],\n[-37.9015533, 175.47851885, \"70\"],\n[-37.9015025333, 175.4779211333, \"72\"],\n[-37.9159910833, 175.4705165167, \"313\"],\n[-37.9138507667, 175.4715170333, \"293\"],\n[-37.9090778667, 175.4737512667, \"229\"],\n[-37.9128195833, 175.4726191, \"282\"],\n[-37.9131350667, 175.4724749667, \"286\"],\n[-37.913967, 175.4720198833, \"294\"],\n[-37.9129905, 175.4725340833, \"284\"],\n[-37.90123535, 175.4772228667, \"67\"],\n[-37.9143479, 175.4719130833, \"298\"],\n[-37.9126444833, 175.4726878, \"280\"],\n[-37.9165660167, 175.4702721167, \"323\"],\n[-37.8989755, 175.4782464167, \"45\"],\n[-37.9093448667, 175.47364355, \"233\"],\n[-37.91447635, 175.4712330833, \"301\"],\n[-37.9095208333, 175.4735786833, \"235\"],\n[-37.9096743333, 175.4741524, \"232\"],\n[-37.89939865, 175.47921515, \"44\"],\n[-37.9151960833, 175.4709698167, \"305\"],\n[-37.9097280833, 175.4745849333, \"228\"],\n[-37.9013794667, 175.47741285, \"69\"],\n[-37.9162268667, 175.47098265, \"318\"],\n[-37.8990613333, 175.4787713333, \"43\"],\n[-37.89780295, 175.4806196167, \"26\"],\n[-37.91541455, 175.4713506833, \"306\"],\n[-37.9097875333, 175.4748558667, \"228A\"],\n[-37.9081311333, 175.4748058, \"196A\"],\n[-37.9150421333, 175.4709822167, \"303\"],\n[-37.9053254667, 175.47610225, \"114\"],\n[-37.9065445333, 175.4749703667, \"173\"],\n[-37.9098205333, 175.4741548833, \"234\"],\n[-37.9081538667, 175.4750639167, \"196\"],\n[-37.9096973, 175.4748021167, \"226A\"],\n[-37.9093885667, 175.4742162667, \"224A\"],\n[-37.9091188, 175.4743508333, \"222A\"],\n[-37.90953665, 175.4733968, \"235A\"],\n[-37.90795735, 175.4748888667, \"194\"],\n[-37.9014723, 175.47869265, \"68\"],\n[-37.9095686833, 175.4733282167, \"237A\"],\n[-37.9090926833, 175.47436235, \"222\"],\n[-37.9012697667, 175.4780105833, \"64\"],\n[-37.9165941667, 175.4699768667, \"325\"],\n[-37.8983101833, 175.4790199333, \"35A\"],\n[-37.8989313833, 175.4795513167, \"38\"],\n[-37.9020688, 175.4782522167, \"74\"],\n[-37.91629055, 175.4698086667, \"321\"],\n[-37.90362685, 175.4769062, \"92\"],\n[-37.8978983167, 175.480096, \"30\"],\n[-37.9095879, 175.4744869833, \"226\"],\n[-37.8988477333, 175.4788966333, \"41\"],\n[-37.9144792667, 175.47185395, \"300\"],\n[-37.90345575, 175.4769852667, \"90\"],\n[-37.90295525, 175.4772238333, \"84\"],\n[-37.9155634, 175.47150915, \"308A\"],\n[-37.9077980667, 175.4749606833, \"192B\"],\n[-37.9024718333, 175.47689145, \"79\"],\n[-37.9010265333, 175.4781286667, \"62\"],\n[-37.9051546167, 175.4761810167, \"108\"],\n[-37.9027743667, 175.4772973, \"82\"],\n[-37.9113692333, 175.4732625, \"242\"],\n[-37.9061175, 175.4761095667, \"140\"],\n[-37.9126536833, 175.4718492, \"283\"],\n[-37.89984655, 175.47884775, \"50\"],\n[-37.8996625, 175.4783593833, \"51\"],\n[-37.9096838, 175.4734820333, \"237\"],\n[-37.9163971333, 175.4703382333, \"319\"],\n[-37.9019659333, 175.47801565, \"74C\"],\n[-37.9017677, 175.4778972667, \"74A\"],\n[-37.9082934833, 175.4747193, \"198\"],\n[-37.9124935167, 175.4721662833, \"279\"],\n[-37.9112822667, 175.4727057, \"263\"],\n[-37.9088314833, 175.4744561333, \"220\"],\n[-37.9140193667, 175.4723065, \"294A\"],\n[-37.9151048833, 175.4715047667, \"302\"],\n[-37.9155721667, 175.4712705333, \"308\"],\n[-37.91564375, 175.4698925833, \"309\"],\n[-37.9157315333, 175.4712060333, \"310\"],\n[-37.9158956833, 175.4711298667, \"312\"],\n[-37.9044821667, 175.4765082167, \"100\"],\n[-37.9045073333, 175.4759204333, \"101\"],\n[-37.9046759167, 175.4758561667, \"103\"],\n[-37.8983034667, 175.4792230333, \"35\"],\n[-37.8987899833, 175.4796567167, \"36\"],\n[-37.89867985, 175.4790447, \"39\"],\n[-37.8990768667, 175.47943105, \"40\"],\n[-37.8981085333, 175.4793068333, \"33\"],\n[-37.89915065, 175.4781128333, \"45A\"],\n[-37.8995490167, 175.4791011167, \"46\"],\n[-37.8997315333, 175.47899365, \"48\"],\n[-37.900319, 175.4785061, \"56\"],\n[-37.9056642333, 175.4759370167, \"118\"],\n[-37.9063413, 175.4756213333, \"150\"],\n[-37.8988354833, 175.47962055, \"36A\"],\n[-37.9029526, 175.4760132333, \"87\"],\n[-37.90312265, 175.4771544, \"86\"],\n[-37.9034438667, 175.4764194333, \"91\"],\n[-37.91418395, 175.4719746167, \"296\"],\n[-37.9140091667, 175.4714322, \"295\"],\n[-37.9141795833, 175.4713487, \"297\"],\n[-37.9138469333, 175.4720815833, \"292\"],\n[-37.91351575, 175.4716747833, \"289\"],\n[-37.9136813667, 175.4716021833, \"291\"],\n[-37.9139259667, 175.4722800333, \"292A\"],\n[-37.9136332, 175.4722156, \"290\"],\n[-37.9093265, 175.4742351667, \"224\"],\n[-37.8977406167, 175.4806423167, \"24\"],\n[-37.9048411167, 175.4757695667, \"109\"],\n[-37.9032051, 175.4765243833, \"89\"],\n[-37.91528, 175.4714141667, \"304\"],\n[-37.9032850667, 175.4770679, \"88\"],\n[-37.9158238167, 175.4703065333, \"313A\"],\n[-37.9064744333, 175.4758051, \"150A\"],\n[-37.9046115, 175.4767234667, \"100A\"],\n[-37.9076618833, 175.4744580667, \"197\"],\n[-37.9076246167, 175.4750397167, \"190\"],\n[-37.9026504333, 175.4768196167, \"79B\"],\n[-37.9051801167, 175.4756040167, \"115\"],\n[-37.89852955, 175.47974005, \"34\"],\n[-37.9075004, 175.47452445, \"187\"],\n[-37.90501535, 175.4756865333, \"111\"],\n[-37.9068534667, 175.4753966333, \"180\"],\n[-37.9029671, 175.4766338333, \"85\"],\n[-37.9065173667, 175.4755659333, \"158\"],\n[-37.9066938833, 175.4754834833, \"166\"],\n[-37.9061558333, 175.4757093333, \"142\"],\n[-37.90567525, 175.4753235167, \"119\"],\n[-37.9124889333, 175.4727737833, \"278\"]\n]\n    ';
var _user$project$Data_ops = _user$project$Data_ops || {};
_user$project$Data_ops['&'] = _elm_lang$core$Json_Decode$map2(
	F2(
		function (x, y) {
			return x(y);
		}));
var _user$project$Data$valueDecoder = function (string) {
	var _p0 = _elm_lang$core$String$toFloat(string);
	if (_p0.ctor === 'Ok') {
		return _elm_lang$core$Json_Decode$succeed(
			_elm_lang$core$Maybe$Just(_p0._0));
	} else {
		return _elm_lang$core$Json_Decode$succeed(_elm_lang$core$Maybe$Nothing);
	}
};
var _user$project$Data$LocationData = F2(
	function (a, b) {
		return {location: a, value: b};
	});
var _user$project$Data$locationDataDecoder = A2(
	_user$project$Data_ops['&'],
	A2(
		_user$project$Data_ops['&'],
		_elm_lang$core$Json_Decode$succeed(_user$project$Data$LocationData),
		A3(
			_elm_lang$core$Json_Decode$map2,
			_klaftertief$elm_slippy_map$SlippyMap_Geo_Location$Location,
			A2(_elm_lang$core$Json_Decode$field, '1', _elm_lang$core$Json_Decode$float),
			A2(_elm_lang$core$Json_Decode$field, '0', _elm_lang$core$Json_Decode$float))),
	A2(
		_elm_lang$core$Json_Decode$field,
		'2',
		A2(_elm_lang$core$Json_Decode$andThen, _user$project$Data$valueDecoder, _elm_lang$core$Json_Decode$string)));
var _user$project$Data$locationData = A2(
	_elm_lang$core$Maybe$withDefault,
	{ctor: '[]'},
	_elm_lang$core$Result$toMaybe(
		A2(
			_elm_lang$core$Json_Decode$decodeString,
			_elm_lang$core$Json_Decode$list(_user$project$Data$locationDataDecoder),
			_user$project$Data$someLocationDataJson)));

var _user$project$Data_World$geoJson = _elm_lang$core$Result$toMaybe(
	A2(_elm_lang$core$Json_Decode$decodeString, _mgold$elm_geojson$GeoJson$decoder, _user$project$Data_World$geoJsonString));

var _user$project$Layer_Debug$tile = F2(
	function (renderState, _p0) {
		var _p1 = _p0;
		var size = _elm_lang$core$Basics$toFloat(renderState.transform.tileSize) * renderState.tileScale;
		return A2(
			_elm_lang$svg$Svg$g,
			{ctor: '[]'},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$svg$Svg$rect,
					{
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$fill('none'),
						_1: {
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$strokeWidth('1'),
							_1: {
								ctor: '::',
								_0: _elm_lang$svg$Svg_Attributes$stroke('#ff0000'),
								_1: {
									ctor: '::',
									_0: _elm_lang$svg$Svg_Attributes$x('0'),
									_1: {
										ctor: '::',
										_0: _elm_lang$svg$Svg_Attributes$y('0'),
										_1: {
											ctor: '::',
											_0: _elm_lang$svg$Svg_Attributes$width(
												_elm_lang$core$Basics$toString(size)),
											_1: {
												ctor: '::',
												_0: _elm_lang$svg$Svg_Attributes$height(
													_elm_lang$core$Basics$toString(size)),
												_1: {ctor: '[]'}
											}
										}
									}
								}
							}
						}
					},
					{ctor: '[]'}),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$svg$Svg$text_,
						{
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$textAnchor('middle'),
							_1: {
								ctor: '::',
								_0: _elm_lang$svg$Svg_Attributes$x(
									_elm_lang$core$Basics$toString(size / 2)),
								_1: {
									ctor: '::',
									_0: _elm_lang$svg$Svg_Attributes$y(
										_elm_lang$core$Basics$toString(size / 2)),
									_1: {ctor: '[]'}
								}
							}
						},
						{
							ctor: '::',
							_0: _elm_lang$svg$Svg$text(
								_elm_lang$core$Basics$toString(_p1)),
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				}
			});
	});
var _user$project$Layer_Debug$layer = A3(_klaftertief$elm_slippy_map$SlippyMap_Layer_Tile$layer, _elm_lang$core$Basics$identity, _user$project$Layer_Debug$tile, _klaftertief$elm_slippy_map$SlippyMap_Layer_Tile$config);

var _user$project$DynamicImageLayer$myGeoJson = {
	ctor: '_Tuple2',
	_0: _mgold$elm_geojson$GeoJson$Geometry(
		_mgold$elm_geojson$GeoJson$Polygon(
			{
				ctor: '::',
				_0: {
					ctor: '::',
					_0: {ctor: '_Tuple3', _0: -6.3281250000000036, _1: 42.032974332441356, _2: 0},
					_1: {
						ctor: '::',
						_0: {ctor: '_Tuple3', _0: 14.414062499999996, _1: 33.431441335575265, _2: 0},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple3', _0: 29.179687499999996, _1: 62.75472592723181, _2: 0},
							_1: {
								ctor: '::',
								_0: {ctor: '_Tuple3', _0: -5.273437500000001, _1: 62.103882522897855, _2: 0},
								_1: {
									ctor: '::',
									_0: {ctor: '_Tuple3', _0: -17.226562500000004, _1: 47.98992166741417, _2: 0},
									_1: {
										ctor: '::',
										_0: {ctor: '_Tuple3', _0: -6.3281250000000036, _1: 42.032974332441356, _2: 0},
										_1: {ctor: '[]'}
									}
								}
							}
						}
					}
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '::',
						_0: {ctor: '_Tuple3', _0: 4.21875, _1: 56.36525013685606, _2: 0},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple3', _0: 1.40625, _1: 46.558860303117164, _2: 0},
							_1: {
								ctor: '::',
								_0: {ctor: '_Tuple3', _0: 16.171875, _1: 55.37911044801047, _2: 0},
								_1: {
									ctor: '::',
									_0: {ctor: '_Tuple3', _0: 4.21875, _1: 56.36525013685606, _2: 0},
									_1: {ctor: '[]'}
								}
							}
						}
					},
					_1: {ctor: '[]'}
				}
			})),
	_1: _elm_lang$core$Maybe$Nothing
};
var _user$project$DynamicImageLayer$markerLayer = A2(
	_klaftertief$elm_slippy_map$SlippyMap_Layer_Marker$simpleLayer,
	_klaftertief$elm_slippy_map$SlippyMap_Layer_Marker$defaultConfig,
	{
		ctor: '::',
		_0: {lon: 6, lat: 50},
		_1: {
			ctor: '::',
			_0: {lon: 7, lat: 51},
			_1: {
				ctor: '::',
				_0: {lon: 8, lat: 52},
				_1: {ctor: '[]'}
			}
		}
	});
var _user$project$DynamicImageLayer$debugLayer = _user$project$Layer_Debug$layer;
var _user$project$DynamicImageLayer$heatmapLayer = A2(
	_klaftertief$elm_slippy_map$SlippyMap_Layer_Heatmap$layer,
	_klaftertief$elm_slippy_map$SlippyMap_Layer_Heatmap$defaultConfig,
	A2(
		_elm_lang$core$List$map,
		function (_p0) {
			var _p1 = _p0;
			return {
				ctor: '_Tuple2',
				_0: _p1.location,
				_1: function (v) {
					return (_elm_lang$core$Basics$isNaN(v) ? 5 : A3(_elm_lang$core$Basics$clamp, 15, 25, v / 10)) / 25;
				}(
					A2(_elm_lang$core$Maybe$withDefault, 10, _p1.value))
			};
		},
		_user$project$Data$locationData));
var _user$project$DynamicImageLayer$geoJsonLayer = A2(
	_klaftertief$elm_slippy_map$SlippyMap_Layer_GeoJson$layer,
	_klaftertief$elm_slippy_map$SlippyMap_Layer_GeoJson$defaultConfig,
	A2(_elm_lang$core$Maybe$withDefault, _user$project$DynamicImageLayer$myGeoJson, _user$project$Data_World$geoJson));
var _user$project$DynamicImageLayer$overlayLayer = A2(
	_klaftertief$elm_slippy_map$SlippyMap_Layer_Overlay$layer,
	_klaftertief$elm_slippy_map$SlippyMap_Layer_Overlay$defaultConfig,
	{
		ctor: '::',
		_0: {
			ctor: '_Tuple2',
			_0: {
				southWest: {lon: -74.22655, lat: 40.712216},
				northEast: {lon: -74.12544, lat: 40.773941}
			},
			_1: 'http://www.lib.utexas.edu/maps/historical/newark_nj_1922.jpg'
		},
		_1: {ctor: '[]'}
	});
var _user$project$DynamicImageLayer$graticuleLayer = _klaftertief$elm_slippy_map$SlippyMap_Layer_Grid$layer(_klaftertief$elm_slippy_map$SlippyMap_Layer_Grid$defaultConfig);
var _user$project$DynamicImageLayer$imageLayer = _klaftertief$elm_slippy_map$SlippyMap_Layer_StaticImage$layer(
	A2(
		_klaftertief$elm_slippy_map$SlippyMap_Layer_StaticImage$withAttribution,
		'© OpenStreetMap contributors',
		A2(
			_klaftertief$elm_slippy_map$SlippyMap_Layer_StaticImage$config,
			'https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png',
			{
				ctor: '::',
				_0: 'a',
				_1: {
					ctor: '::',
					_0: 'b',
					_1: {
						ctor: '::',
						_0: 'c',
						_1: {ctor: '[]'}
					}
				}
			})));
var _user$project$DynamicImageLayer$Model = function (a) {
	return {mapState: a};
};
var _user$project$DynamicImageLayer$init = function (_p2) {
	var _p3 = _p2;
	return A2(
		_elm_lang$core$Platform_Cmd_ops['!'],
		_user$project$DynamicImageLayer$Model(
			A2(
				_klaftertief$elm_slippy_map$SlippyMap_Interactive$resize,
				{ctor: '_Tuple2', _0: _p3.width, _1: _p3.height},
				A2(
					_klaftertief$elm_slippy_map$SlippyMap_Interactive$center,
					{lon: 7, lat: 51},
					6))),
		{ctor: '[]'});
};
var _user$project$DynamicImageLayer$Resize = function (a) {
	return {ctor: 'Resize', _0: a};
};
var _user$project$DynamicImageLayer$MapMsg = function (a) {
	return {ctor: 'MapMsg', _0: a};
};
var _user$project$DynamicImageLayer$mapConfig = _klaftertief$elm_slippy_map$SlippyMap_Interactive$config(_user$project$DynamicImageLayer$MapMsg);
var _user$project$DynamicImageLayer$update = F2(
	function (msg, model) {
		var _p4 = msg;
		if (_p4.ctor === 'MapMsg') {
			var newMapState = A3(_klaftertief$elm_slippy_map$SlippyMap_Interactive$update, _user$project$DynamicImageLayer$mapConfig, _p4._0, model.mapState);
			return A2(
				_elm_lang$core$Platform_Cmd_ops['!'],
				_user$project$DynamicImageLayer$Model(newMapState),
				{ctor: '[]'});
		} else {
			return A2(
				_elm_lang$core$Platform_Cmd_ops['!'],
				_elm_lang$core$Native_Utils.update(
					model,
					{
						mapState: A2(
							_klaftertief$elm_slippy_map$SlippyMap_Interactive$resize,
							{ctor: '_Tuple2', _0: _p4._0.width, _1: _p4._0.height},
							model.mapState)
					}),
				{ctor: '[]'});
		}
	});
var _user$project$DynamicImageLayer$view = function (model) {
	return A2(
		_elm_lang$html$Html$div,
		{ctor: '[]'},
		{
			ctor: '::',
			_0: A3(
				_klaftertief$elm_slippy_map$SlippyMap_Interactive$view,
				_user$project$DynamicImageLayer$mapConfig,
				model.mapState,
				{
					ctor: '::',
					_0: _user$project$DynamicImageLayer$imageLayer,
					_1: {
						ctor: '::',
						_0: _user$project$DynamicImageLayer$overlayLayer,
						_1: {
							ctor: '::',
							_0: _user$project$DynamicImageLayer$markerLayer,
							_1: {ctor: '[]'}
						}
					}
				}),
			_1: {ctor: '[]'}
		});
};
var _user$project$DynamicImageLayer$subscriptions = function (model) {
	return _elm_lang$core$Platform_Sub$batch(
		{
			ctor: '::',
			_0: A2(_klaftertief$elm_slippy_map$SlippyMap_Interactive$subscriptions, _user$project$DynamicImageLayer$mapConfig, model.mapState),
			_1: {
				ctor: '::',
				_0: _elm_lang$window$Window$resizes(_user$project$DynamicImageLayer$Resize),
				_1: {ctor: '[]'}
			}
		});
};
var _user$project$DynamicImageLayer$main = _elm_lang$html$Html$programWithFlags(
	{init: _user$project$DynamicImageLayer$init, view: _user$project$DynamicImageLayer$view, update: _user$project$DynamicImageLayer$update, subscriptions: _user$project$DynamicImageLayer$subscriptions})(
	A2(
		_elm_lang$core$Json_Decode$andThen,
		function (height) {
			return A2(
				_elm_lang$core$Json_Decode$andThen,
				function (width) {
					return _elm_lang$core$Json_Decode$succeed(
						{height: height, width: width});
				},
				A2(_elm_lang$core$Json_Decode$field, 'width', _elm_lang$core$Json_Decode$int));
		},
		A2(_elm_lang$core$Json_Decode$field, 'height', _elm_lang$core$Json_Decode$int)));

var Elm = {};
Elm['DynamicImageLayer'] = Elm['DynamicImageLayer'] || {};
if (typeof _user$project$DynamicImageLayer$main !== 'undefined') {
    _user$project$DynamicImageLayer$main(Elm['DynamicImageLayer'], 'DynamicImageLayer', undefined);
}

if (typeof define === "function" && define['amd'])
{
  define([], function() { return Elm; });
  return;
}

if (typeof module === "object")
{
  module['exports'] = Elm;
  return;
}

var globalElm = this['Elm'];
if (typeof globalElm === "undefined")
{
  this['Elm'] = Elm;
  return;
}

for (var publicModule in Elm)
{
  if (publicModule in globalElm)
  {
    throw new Error('There are two Elm modules called `' + publicModule + '` on this page! Rename one of them.');
  }
  globalElm[publicModule] = Elm[publicModule];
}

}).call(this);
